#include <vector>
#include <array>
#include <stdexcept>
#include <algorithm>

template<typename T>
class Deque {
    using size_t = std::size_t;

    static const size_t chunk_size = 1024;
    static const size_t chunk_bytes = chunk_size * sizeof(T);

    std::vector<T*> a;
    size_t head; // [
    size_t tail; // )

    void swap(Deque& other) {
        std::swap(a, other.a);
        std::swap(head, other.head);
        std::swap(tail, other.tail);
    }

    // chunks_cnt >= a.size() && (head <= tail || head / chunk_size != (tail - 1) / chunk_size)
    void expand(size_t chunks_cnt) {
        if (chunks_cnt == 0 || chunks_cnt == a.size()) {
            return;
        }
        size_t elem_cnt = size();
        size_t old_chunks_cnt = a.size();
        size_t start_chunk = head / chunk_size;
        std::rotate(a.begin(), a.begin() + static_cast<typename iterator::difference_type>(start_chunk), a.end());

        head -= start_chunk * chunk_size;
        tail = (head + elem_cnt) % (chunks_cnt * chunk_size);

        a.resize(chunks_cnt);
        size_t block = old_chunks_cnt;
        try {
            for (block = old_chunks_cnt; block < a.size(); ++block) {
                a[block] = static_cast<T*>(operator new(chunk_bytes));
            }
        } catch (...) {
            for (size_t j = block; j > old_chunks_cnt; --j) {
                operator delete(a[j - 1]);
            }
            throw;
        }
    }

    void fillingConstructorImpl(const size_t size, const T& value) {
        size_t chunks_cnt = (size + chunk_size - 1) / chunk_size;
        expand(chunks_cnt * 2 + 1);
        size_t cnt = 0;
        size_t block = 0;
        T* ptr = a[block];
        try {
            for (block = 0; block < a.size(); ++block) {
                for (ptr = a[block]; ptr < a[block] + chunk_size; ++ptr) {
                    new (ptr) T(value);
                    ++cnt;
                    if (cnt >= size) {
                        break;
                    }
                }
            }
        } catch (...) {
            for (size_t k = block + 1; k > 0; --k) {
                while (ptr > a[k - 1]) {
                    --ptr;
                    ptr->~T();
                }
                if (k - 1 > 0) {
                    ptr = a[k - 2] + chunk_size;
                }
            }
            for (auto block_ptr : a) {
                operator delete(block_ptr);
            }
            throw;
        }
    }

public:
    using value_type = T;

    template<typename V>
    class BaseIterator {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using value_type = V;
        using pointer = value_type*;
        using reference = value_type&;

    private:
        V *const * chunks;
        size_t local_pos;
        size_t chunk_pos;
        V* chunk;
        size_t cap;
        const size_t * head;

        size_t get_pos() const {
            return chunk_pos * chunk_size + local_pos;
        }

        size_t get_id() const {
            size_t pos = get_pos();
            return pos >= *head ? pos - *head : (cap - *head) + pos;
        }

        void add(size_t diff) {
            if (local_pos + diff < chunk_size) {
                local_pos += diff;
            } else {
                size_t pos = get_pos();
                size_t new_pos = (pos + diff) % cap;
                chunk_pos = new_pos / chunk_size;
                local_pos = new_pos % chunk_size;
                chunk = chunks[chunk_pos];
            }
        }

        void subtract(size_t diff) {
            if (local_pos >= diff) {
                local_pos -= diff;
            } else {
                size_t pos = get_pos();
                size_t new_pos = (pos + cap - diff) % cap;
                chunk_pos = new_pos / chunk_size;
                local_pos = new_pos % chunk_size;
                chunk = chunks[chunk_pos];
            }
        }

        void swap(BaseIterator& other) {
            std::swap(chunks, other.chunks);
            std::swap(local_pos, other.local_pos);
            std::swap(chunk_pos, other.chunk_pos);
            std::swap(chunk, other.chunk);
            std::swap(cap, other.cap);
            std::swap(head, other.head);
        }

    public:
        BaseIterator(const Deque &deque, size_t id): chunks(deque.a.data()),
                                                     local_pos(id % chunk_size),
                                                     chunk_pos(id / chunk_size),
                                                     chunk(deque.a.empty() ? nullptr : deque.a[chunk_pos]),
                                                     cap(deque.capacity()),
                                                     head(&deque.head) {}

        BaseIterator(const BaseIterator& it): chunks(it.chunks),
                                              local_pos(it.local_pos),
                                              chunk_pos(it.chunk_pos),
                                              chunk(it.chunk),
                                              cap(it.cap),
                                              head(it.head) {}

        ~BaseIterator() = default;


        operator BaseIterator<const T>() {
            return reinterpret_cast<BaseIterator<T>>(*this);
        }

        BaseIterator& operator=(const BaseIterator& it) {
            BaseIterator copy(it);
            swap(copy);
            return *this;
        }

        V& operator*() const {
            return *(chunk + local_pos);
        }

        V* operator->() const {
            return chunk + local_pos;
        }

        BaseIterator& operator++() {
            if (cap == 0) {
                return *this;
            }
            if (local_pos + 1 == chunk_size) {
                ++chunk_pos;
                chunk_pos %= (cap / chunk_size);
                local_pos = 0;
                chunk = chunks[chunk_pos];
            } else {
                ++local_pos;
            }
            return *this;
        }

        BaseIterator& operator--() {
            if (cap == 0) {
                return *this;
            }
            if (local_pos == 0) {
                chunk_pos = (chunk_pos == 0 ? cap / chunk_size - 1 : chunk_pos - 1);
                local_pos = chunk_size - 1;
                chunk = chunks[chunk_pos];
            } else {
                --local_pos;
            }
            return *this;
        }

        BaseIterator operator++(int) {
            auto copy = *this;
            ++(*this);
            return copy;
        }

        BaseIterator operator--(int) {
            auto copy = *this;
            --(*this);
            return copy;
        }

        BaseIterator& operator+=(difference_type diff) {
            if (cap == 0) {
                return *this;
            }
            if (diff >= 0) {
                add(static_cast<size_t>(diff));
            } else {
                subtract(static_cast<size_t>(-diff));
            }
            return *this;
        }

        BaseIterator& operator-=(difference_type diff) {
            if (cap == 0) {
                return *this;
            }
            if (diff >= 0) {
                subtract(static_cast<size_t>(diff));
            } else {
                add(static_cast<size_t>(-diff));
            }
            return *this;
        }

        BaseIterator operator+(difference_type diff) const {
            auto copy = *this;
            copy += diff;
            return copy;
        }

        BaseIterator operator-(difference_type diff) const {
            auto copy = *this;
            copy -= diff;
            return copy;
        }

        bool operator==(const BaseIterator& rhs) const {
            return chunk_pos == rhs.chunk_pos && local_pos == rhs.local_pos;
        }

        bool operator!=(const BaseIterator& rhs) const {
            return !(*this == rhs);
        }

        bool operator<(const BaseIterator& rhs) const {
            return (rhs + 1).get_id() != 0 && get_id() < rhs.get_id();
        }

        bool operator>(const BaseIterator& rhs) const {
            return rhs < *this;
        }

        bool operator<=(const BaseIterator& rhs) const {
            return (*this == rhs) || (*this < rhs);
        }

        bool operator>=(const BaseIterator& rhs) const {
            return (*this == rhs) || (rhs < *this);
        }

        ptrdiff_t operator-(const BaseIterator& rhs) const {
            auto id = get_id();
            auto rhs_id = rhs.get_id();
            return id >= rhs_id ? static_cast<ptrdiff_t>(id - rhs_id) : -static_cast<ptrdiff_t>(rhs_id - id);
        }
    };

    using iterator = BaseIterator<T>;
    using const_iterator = BaseIterator<const T>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    iterator begin() {
        return iterator(*this, head);
    }

    const_iterator begin() const {
        return const_iterator(*this, head);
    }

    const_iterator cbegin() const {
        return const_iterator(*this, head);
    }

    iterator end() {
        return iterator(*this, tail);
    }

    const_iterator end() const {
        return const_iterator(*this, tail);
    }

    const_iterator cend() const {
        return const_iterator(*this, tail);
    }

    auto rbegin() {
        return std::reverse_iterator(end());
    }

    auto rbegin() const {
        return std::reverse_iterator(end());
    }

    auto crbegin() const {
        return std::reverse_iterator(cend());
    }

    auto rend() {
        return std::reverse_iterator(begin());
    }

    auto rend() const {
        return std::reverse_iterator(begin());
    }

    auto crend() const {
        return std::reverse_iterator(cbegin());
    }

    Deque(): head(0), tail(0) {}

    Deque(const Deque<T>& other): head(other.head), tail(other.tail) {
        size_t cnt = 0;
        expand(other.a.size());
        size_t block = 0;
        size_t id = 0;
        T* ptr = 0;
        try {
            for (block = 0; block < a.size(); ++block) {
                ptr = a[block];
                for (id = 0; id < chunk_size; ++id) {
                    new (ptr) T(other.a[block][id]);
                    ++ptr;
                    ++cnt;
                    if (cnt >= other.size()) {
                        break;
                    }
                }
            }
        } catch(...) {
            for (size_t k = block + 1; k > 0; --k) {
                while (id > 0) {
                    --id;
                    --ptr;
                    ptr->~T();
                }
                id = chunk_size;
                if (k - 1 > 0) {
                    ptr = a[k - 2] + chunk_size;
                }
            }
            for (auto block_ptr : a) {
                operator delete(block_ptr);
            }
            throw;
        }
    }

    Deque(const size_t size, const T& value): head(0), tail(size) {
        fillingConstructorImpl(size, value);
    }

    explicit Deque(const size_t size): head(0), tail(size) {
        fillingConstructorImpl(size, T());
    }

    ~Deque() {
        if (a.empty()) {
            return;
        }
        for (size_t i = head; i != tail; i = (i + 1) % capacity()) {
            size_t chunk = i / chunk_size;
            size_t local = i % chunk_size;
            (a[chunk] + local)->~T();
        }
        for (size_t i = 0; i < a.size(); ++i) {
            operator delete(a[i]);
        }
    }

    Deque& operator=(const Deque& other) {
        if (a.data() == other.a.data()) {
            return *this;
        }
        Deque cp(other);
        swap(cp);
        return *this;
    }

    size_t capacity() const {
        return a.size() * chunk_size;
    }

    size_t size() const {
        return head <= tail ? tail - head : tail + (capacity() - head);
    }

    const T& operator[](size_t pos) const {
        pos += head;
        if (capacity() != 0) {
            pos %= capacity();
        }
        return a[pos / chunk_size][pos % chunk_size];
    }

    T& operator[](size_t pos) {
        pos += head;
        if (capacity() != 0) {
            pos %= capacity();
        }
        return a[pos / chunk_size][pos % chunk_size];
    }

    const T& at(size_t pos) const {
        if (pos >= size()) {
            throw std::out_of_range("at(size_t) argument is out of range");
        } else {
            return (*this)[pos];
        }
    }

    T& at(size_t pos) {
        if (pos >= size()) {
            throw std::out_of_range("at(size_t) argument is out of range");
        } else {
            return (*this)[pos];
        }
    }

    void push_back(const T& rhs) {
        if (capacity() == 0 || (tail + 1) % capacity() == head ||
            (tail % chunk_size == 0 && tail / chunk_size == head / chunk_size)) {
            expand(2 * a.size() + 1);
        }
        new (a[tail / chunk_size] + (tail % chunk_size)) T(rhs);
        ++tail;
        tail %= capacity();
    }

    void pop_back() {
        size_t new_tail = (tail == 0 ? capacity() - 1 : tail - 1);
        (a[new_tail / chunk_size] + (new_tail % chunk_size))->~T();
        tail = new_tail;
    }

    void push_front(const T& rhs) {
        if (capacity() == 0 || (tail + 1) % capacity() == head ||
            (head % chunk_size == 0 && head / chunk_size == (tail / chunk_size + 1) % a.size())) {
            expand(2 * a.size() + 1);
        }
        size_t new_head = (head == 0 ? capacity() - 1 : head - 1);
        new (a[new_head / chunk_size] + (new_head % chunk_size)) T(rhs);
        head = new_head;
    }

    void pop_front() {
        (a[head / chunk_size] + (head % chunk_size))->~T();
        ++head;
        head %= capacity();
    }

    iterator insert(iterator it, const T& val) {
        if (it == end()) {
            push_back(val);
            return end() - 1;
        }
        ptrdiff_t pos = it - begin();
        T shift = *it;
        try {
            *it = val;
            push_back(*(end() - 1));
        } catch (...) {
            *it = shift;
            throw;
        }
        auto end_it = end();
        while (++it != end_it) {
            std::swap(shift, *it);
        }
        return begin() + pos;
    }

    void erase(iterator elem_it) {
        auto it = --end();
        T shift = *it;
        while (--it >= elem_it) {
            std::swap(shift, *it);
        }
        pop_back();
    }
}
;
