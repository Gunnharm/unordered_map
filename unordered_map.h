#include <algorithm>
#include <memory>
#include <vector>
#include <list>
#include <iterator>
#include <cassert>

template<class KeyType, class ValueType, class Hash = std::hash<KeyType> >
class HashMap {
protected:
    static constexpr double MAX_LOAD_FACTOR = 0.2l;
    static constexpr uint32_t MINIMUM_TABLE_SIZE = 4; //Must be at least 1/MAX_LOAD_FACTOR

    struct Node;

    typedef std::list<size_t> LsType;

    struct Node {
        std::pair<const KeyType, ValueType> key_val;
        LsType::iterator lsIterator;
        bool tombstone;

        Node(const KeyType &key, const ValueType &val) : key_val(make_pair(key, val)), tombstone(false) {}

        explicit Node(const std::pair<const KeyType, ValueType> &key_val_) : key_val(key_val_), tombstone(false) {}

        Node(const KeyType &key, const ValueType &val, LsType::iterator lsIterator_) : key_val(make_pair(key, val)),
                                                                                       lsIterator(lsIterator_),
                                                                                       tombstone(false) {}

        Node(const std::pair<const KeyType, ValueType> &key_val_, LsType::iterator lsIterator_) : key_val(key_val_),
                                                                                                  lsIterator(
                                                                                                          lsIterator_),
                                                                                                  tombstone(false) {}

        Node(const std::pair<KeyType, ValueType> &key_val_, LsType::iterator lsIterator_) : key_val(key_val_),
                                                                                            lsIterator(
                                                                                                    lsIterator_),
                                                                                            tombstone(false) {}

    };

    typedef std::vector<Node *> TableType;

    uint64_t IntHash(uint64_t x) const {
        x = ((x + 1 ) ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
        x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
        x = x ^ (x >> 31);
        return x;
    }

    uint64_t GetNextProbe(uint64_t val_hash, uint64_t prev_probe) const {
        return IntHash(val_hash + prev_probe);
    }

public:


    class iterator {
    public:

        iterator() : parent_table(nullptr), it(nullptr) {}

        explicit iterator(TableType *parent_table_, typename LsType::iterator it_)
                : parent_table(parent_table_), it(it_) {}

        std::pair<const KeyType, ValueType> &operator*() {
            return (*parent_table)[*it]->key_val;
        }

        std::pair<const KeyType, ValueType> *operator->() {
            return &((*parent_table)[*it]->key_val);
        }

        const std::pair<const KeyType, ValueType> &operator*() const {
            return (*parent_table)[*it]->key_val;
        }

        const std::pair<const KeyType, ValueType> *operator->() const {
            return &((*parent_table)[*it]->keey_val);
        }

        friend bool operator==(const iterator lhs, const iterator rhs) {
            return lhs.it == rhs.it && lhs.parent_table == rhs.parent_table;
        }

        friend bool operator!=(const iterator lhs, const iterator rhs) {
            return lhs.it != rhs.it || lhs.it != rhs.it;
        }

        iterator &operator++() {
            ++it;
            return *this;
        }

        iterator operator++(int) {
            iterator copy(*this);
            ++it;
            return copy;
        }


    protected:
        TableType *parent_table;
        typename LsType::iterator it;
    };

    class const_iterator {
    public:
        friend iterator;

        const_iterator() : parent_table(nullptr), it(nullptr) {}

        const_iterator(const iterator a) : parent_table(a.parent_table), it(a.it) {}

        explicit const_iterator(const TableType *parent_table_,
                                typename LsType::const_iterator it_) : parent_table(parent_table_), it(it_) {}

        const std::pair<const KeyType, ValueType> &operator*() const {
            return (*parent_table)[*it]->key_val;
        }

        const std::pair<const KeyType, ValueType> *operator->() const {
            return &((*parent_table)[*it]->key_val);
        }

        friend bool operator==(const const_iterator lhs, const const_iterator rhs) {
            return lhs.it == rhs.it && lhs.parent_table == rhs.parent_table;
        }

        friend bool operator!=(const const_iterator lhs, const const_iterator rhs) {
            return lhs.it != rhs.it || lhs.parent_table != rhs.parent_table;
        }

        const_iterator &operator++() {
            ++it;
            return *this;
        }

        const_iterator operator++(int) {
            const_iterator copy(*this);
            ++it;
            return copy;
        }

    protected:
        const TableType *parent_table;
        typename LsType::const_iterator it;
    };


    explicit HashMap(Hash hasher_ = Hash()) : hasher(hasher_), table(MINIMUM_TABLE_SIZE), sz(0), touched_sz(0) {}

    HashMap(std::initializer_list<std::pair<KeyType, ValueType>> ls, Hash hasher_ = Hash()) : hasher(
            hasher_), table(MINIMUM_TABLE_SIZE), sz(0), touched_sz(0) {
        for (const auto &pr: ls) {
            insert(pr);
        }
    }

    HashMap(const HashMap<KeyType, ValueType, Hash>& copy): HashMap(copy.hash_function()) {
        for (auto& pr : copy) {
            insert(pr);
        }
    }

    HashMap& operator=(const HashMap<KeyType, ValueType, Hash>& copy) {
        for (auto& pr : copy) {
            insert(pr);
        }
        return *this;
    }

    template<class InputIt>
    HashMap(InputIt first, InputIt last, Hash hasher_ = Hash()) : hasher(hasher_), table(MINIMUM_TABLE_SIZE), sz(0), touched_sz(0) {
        while (first != last) {
            insert(*first);
            ++first;
        }
    }

    ~HashMap() {
        clear();
    }


    size_t size() const {
        return sz;
    }

    bool empty() const {
        return sz == 0;
    }

    Hash hash_function() const {
        return hasher;
    }

    //Reserves at least capacity number of buckets(with 1 element) and regenerates hash table
    void Rehash(size_t capacity) {
        if (capacity > table.size()) {
            LsType newLs;
            TableType newTable(capacity);
            for (size_t id = 0; id < table.size(); ++id) {
                if (table[id]) {
                    if (table[id]->tombstone) {
                        delete table[id];
                        table[id] = nullptr;
                    } else {
                        const std::pair<KeyType, ValueType> &pr = table[id]->key_val;
                        uint64_t val_hash = hasher(pr.first);
                        uint64_t probe = val_hash;
                        while (newTable[probe % capacity]) {
                            probe = GetNextProbe(val_hash, probe);
                        }
                        size_t new_id = probe % capacity;
                        newLs.push_front(new_id);
                        newTable[new_id] = table[id];
                        table[id] = nullptr;
                        newTable[new_id]->lsIterator = newLs.begin();
                    }
                }
            }
            alive_nodes.clear();
            touched_sz = sz;
            table.swap(newTable);
            alive_nodes.swap(newLs);
        }
    }

    iterator insert(const std::pair<KeyType, ValueType> &pr) {
        auto it_find = find(pr.first);
        if (it_find != end()) return it_find;
        uint64_t val_hash = hasher(pr.first);
        uint64_t probe = val_hash;
        while (table[probe % table.size()] && !table[probe % table.size()]->tombstone) {
            probe = GetNextProbe(val_hash, probe);
        }
        size_t id = probe % table.size();
        if (table[id]) {
            delete table[id];
            table[id] = nullptr;
        } else {
            ++touched_sz;
        }
        alive_nodes.push_front(id);
        table[id] = new Node(pr, alive_nodes.begin());
        iterator ret = iterator(&table, alive_nodes.begin());
        ++sz;
        if (touched_sz > MAX_LOAD_FACTOR * table.size()) {
            Rehash(table.size() * 2);
            ret = find(pr.first);
        }
        return ret;
    }

    void erase(const KeyType &key) {
        uint64_t val_hash = hasher(key);
        uint64_t probe = val_hash;
        while (table[probe % table.size()] && (table[probe % table.size()]->tombstone ||
                                               !(table[probe % table.size()]->key_val.first == key))) {
            probe = GetNextProbe(val_hash, probe);
        }
        size_t id = probe % table.size();
        if (table[id]) {
            alive_nodes.erase(table[id]->lsIterator);
            table[id]->tombstone = true;
            --sz;
        }
    }

    iterator begin() {
        return iterator(&table, alive_nodes.begin());
    }

    const_iterator begin() const {
        return const_iterator(&table, alive_nodes.cbegin());
    }

    iterator end() {
        return iterator(&table, alive_nodes.end());
    }

    const_iterator end() const {
        return const_iterator(&table, alive_nodes.cend());
    }


    iterator find(const KeyType &key) {
        uint64_t val_hash = hasher(key);
        uint64_t probe = val_hash;
        while (table[probe % table.size()] && (table[probe % table.size()]->tombstone ||
                                               !(table[probe % table.size()]->key_val.first == key))) {
            probe = GetNextProbe(val_hash, probe);
        }
        if (table[probe % table.size()]) {
            return iterator(&table, table[probe % table.size()]->lsIterator);
        } else {
            return end();
        }
    }

    const_iterator find(const KeyType &key) const {
        uint64_t val_hash = hasher(key);
        uint64_t probe = val_hash;
        while (table[probe % table.size()] && (table[probe % table.size()]->tombstone ||
                                               !(table[probe % table.size()]->key_val.first == key))) {
            probe = GetNextProbe(val_hash, probe);
        }
        if (table[probe % table.size()]) {
            return const_iterator(&table, table[probe % table.size()]->lsIterator);
        } else {
            return end();
        }
    }

    ValueType &operator[](const KeyType &key) {
        iterator it = find(key);
        if (it == end()) {
            it = insert(std::make_pair(key, ValueType()));
        }
        return it->second;

    }

    const ValueType &at(const KeyType &key) const {
        const_iterator it = find(key);
        if (it == end()) {
            throw std::out_of_range("HashMap At error");
        }
        return it->second;
    }

    void clear() {
        for (size_t id = 0; id < table.size(); ++id) {
            if (table[id]) {
                delete table[id];
            }
            table[id] = nullptr;
        }
        alive_nodes.clear();
        sz = 0;
        touched_sz = 0;
    }

protected:
public:
    Hash hasher;
    TableType table;
    LsType alive_nodes;
    size_t sz{};
    size_t touched_sz{};

};
