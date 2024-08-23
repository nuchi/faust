
#ifndef __NONCOMPPTR__
#define __NONCOMPPTR__

#include <map>
#include <ostream>


template <typename T> class NonComparablePtrBase;


template <typename T>
class __attribute__((trivial_abi)) NonComparablePtr {
    T* fPtr;
    // bool operator<(const NonComparablePtr<T>& other) const { return fPtr < other.fPtr; };

  public:
    NonComparablePtr() = default;
    // ~NonComparablePtr() = default;
    // NonComparablePtr(const NonComparablePtr&) = default;
    // NonComparablePtr& operator=(const NonComparablePtr&) = default;
    // NonComparablePtr(NonComparablePtr&&) = default;
    // NonComparablePtr& operator=(NonComparablePtr&&) = default;

    explicit NonComparablePtr(T* ptr) noexcept : fPtr(ptr) {
        static_assert(std::is_base_of<NonComparablePtrBase<T>, T>::value, "didn't subclass NonComparablePtrBase");
    };

    NonComparablePtr(std::nullptr_t n) noexcept : fPtr(nullptr) {}

    explicit operator void*() { return (void*)fPtr; }
    explicit operator void const*() const { return (void*)fPtr; }

    T& operator* () {check();
        return *fPtr;
    }

    T* operator->() {check();
        return fPtr;
    }

    const T& operator* () const {check();
        return *fPtr;
    }

    const T* operator->() const {check();
        return fPtr;
    }

    const T& operator [] (int i) const {check();return fPtr[i];}
    T& operator [] (int i) {check();return fPtr[i];}


    void check() const {
        if (fPtr == nullptr) {
            __builtin_trap();
        }
    }

    bool operator==(const NonComparablePtr<T>& other) const {
        return fPtr == other.fPtr;
    }

    bool operator==(std::nullptr_t n) const {
        return fPtr == nullptr;
    }

    operator bool() const {
        return bool(fPtr);
    }

    bool operator>(const NonComparablePtr<T>& other) const = delete;
    bool operator<=(const NonComparablePtr<T>& other) const = delete;
    bool operator>=(const NonComparablePtr<T>& other) const = delete;

    // Need to define explicitly for each instantiation
    bool operator<(const NonComparablePtr<T>& other) const;

    friend std::ostream& operator<< (std::ostream& stream, const NonComparablePtr<T>& ncptr) {
        return stream << ncptr.fPtr;
    }

    friend class std::hash<NonComparablePtr<T>>;
};


template <typename T>
class NonComparablePtrBase {
  public:
    virtual ~NonComparablePtrBase(){}
    NonComparablePtr<T> operator&() { return NonComparablePtr<T>(reinterpret_cast<T*>(this)); }
    const NonComparablePtr<T> operator&() const { return NonComparablePtr<T>(reinterpret_cast<T*>(this)); }
};


template <class T>
struct std::hash<NonComparablePtr<T>>
{
  std::size_t operator()(const NonComparablePtr<T>& ncptr) const
  {
    return std::hash<T*>{}(ncptr.fPtr);
  }
};

#endif
