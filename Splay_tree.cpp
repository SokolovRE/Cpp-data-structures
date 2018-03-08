#include <iostream>
#include <list>
#include <algorithm>

template<class ValueType>
struct Node{
    typename std::list<ValueType>::const_iterator data;
    Node* left = nullptr;
    Node* right = nullptr;
    Node* parent = nullptr;
};

template<class ValueType>
class Set{
 public:
    Set()
    : root(nullptr)
    {}

    template<class iterator>
    Set(iterator begin, iterator end)
    : root(nullptr) {
        for (; begin != end; ++begin) {
            insert(*begin);
        }
    }

    Set(std::initializer_list<ValueType> elements_list)
    : root(nullptr) {
        for (auto list_iterator = elements_list.begin()
            ; list_iterator != elements_list.end(); ++list_iterator) {
            insert(*list_iterator);
        }
    }

    Set(const Set& other)
    : root(nullptr) {
        *this = other;
    }

    Set& operator=(const Set& other) {
        if (root == other.root) {
            return *this;
        }
        if (root != nullptr) {
            clear(root);
            root = nullptr;
        }
        std::list<ValueType> temp(other.data_list);
        std::swap(data_list, temp);
        if (other.root != nullptr) {
            auto iter = begin();
            root = vertex_copy(other.root, iter);
        }
        return *this;
    }

    size_t size() const {
        return data_list.size();
    }

    bool empty() const {
        return data_list.empty();
    }

    void insert(ValueType element) {
        Node<ValueType>* new_left;
        Node<ValueType>* new_right;
        new_left = nullptr;
        new_right = nullptr;
        split(element, new_left, new_right);
        if (new_right == nullptr
        || !(!(*(new_right->data) < element)
        && !(element < *(new_right->data)))) {
            Node<ValueType>* new_root = new Node<ValueType>;
            if (new_right != nullptr) {
                auto iter = new_right->data;
                data_list.insert(iter, element);
                --iter;
                new_root->data = iter;
            } else {
                data_list.push_back(element);
                auto iter = end();
                --iter;
                new_root->data = iter;
            }
            new_root->left = new_left;
            new_root->right = new_right;
            if (new_left != nullptr) {
                new_left->parent = new_root;
            }
            if (new_right != nullptr) {
                new_right->parent = new_root;
            }
            root = new_root;
        } else {
            if (new_right == nullptr) {
                root = new_left;
            } else if (new_left == nullptr) {
                root = new_right;
            } else {
                new_right->left = new_left;
                new_left->parent = new_right;
                root = new_right;
            }
        }
    }

    void erase(ValueType element) {
        if (find(element) != end()) {
            Node<ValueType>* temp = root;
            data_list.erase(root->data);
            if (root->left != nullptr) {
                root->left->parent = nullptr;
            }
            if (root->right != nullptr) {
                root->right->parent = nullptr;
            }
            root = merge(root->left, root->right);
            delete temp;
        }
    }

    typedef typename std::list<ValueType>::const_iterator iterator;

    iterator begin() const {
        return data_list.cbegin();
    }

    iterator end() const {
        return data_list.cend();
    }

    iterator find(ValueType element) const {
        Node<ValueType>* temp = root;
        Node<ValueType>* vertex = root;
        while (vertex != nullptr) {
            if (!(*(vertex->data) < element)
                && !(element < *(vertex->data))) {
                splay(vertex);
                return root->data;
            }
            temp = vertex;
            if (*(vertex->data) < element) {
                vertex = vertex->right;
            } else {
                vertex = vertex->left;
            }
        }
        splay(temp);
        return end();
    }

    iterator lower_bound(ValueType element) const {
        Node<ValueType>* temp_2 = root;
        Node<ValueType>* temp = nullptr;
        Node<ValueType>* vertex = root;
        while (vertex != nullptr) {
            if (!(*(vertex->data) < element)
                && !(element < *(vertex->data))) {
                splay(vertex);
                return root->data;
            }
            temp_2 = vertex;
            if (*(vertex->data) < element) {
                vertex = vertex->right;
            } else {
                temp = vertex;
                vertex = vertex->left;
            }
        }
        if (temp == nullptr) {
            if (temp_2 != nullptr) {
                splay(temp_2);
            }
            return end();
        } else {
            splay(temp);
            return root->data;
        }
    }

    ~Set() {
        clear(root);
    }

 private:
    mutable Node<ValueType>* root;
    std::list<ValueType> data_list;

    void rotate(Node<ValueType>*& vertex) const {
        if (vertex->parent != nullptr) {
            if (vertex->parent->parent != nullptr) {
                if (vertex->parent->parent->left == vertex->parent) {
                    vertex->parent->parent->left = vertex;
                } else {
                    vertex->parent->parent->right = vertex;
                }
            }
            if (vertex->parent->left == vertex) {
                vertex->parent->left = vertex->right;
                if (vertex->right != nullptr) {
                    vertex->right->parent = vertex->parent;
                }
                vertex->right = vertex->parent;
                vertex->parent = vertex->right->parent;
                vertex->right->parent = vertex;
            } else {
                vertex->parent->right = vertex->left;
                if (vertex->left != nullptr) {
                    vertex->left->parent = vertex->parent;
                }
                vertex->left = vertex->parent;
                vertex->parent = vertex->left->parent;
                vertex->left->parent = vertex;
            }
        }
    }

    void splay(Node<ValueType>* vertex) const {
        if (vertex == nullptr || vertex->parent == nullptr) {
            root = vertex;
            return;
        }
        if (vertex->parent->parent != nullptr) {
            if ((vertex->parent->parent->left == vertex->parent &&
                 vertex->parent->left == vertex) ||
                (vertex->parent->parent->right == vertex->parent &&
                 vertex->parent->right == vertex)) {
                rotate(vertex->parent);
                rotate(vertex);
            } else {
                rotate(vertex);
                rotate(vertex);
            }
        } else {
            rotate(vertex);
        }
        splay(vertex);
    }

    void clear(Node<ValueType>* vertex) {
        if (vertex == nullptr) {
            return;
        }
        clear(vertex->left);
        clear(vertex->right);
        data_list.erase(vertex->data);
        delete vertex;
    }

    Node<ValueType>* vertex_copy(const Node<ValueType>* other
        , iterator& iter
        , Node<ValueType>* vertex_parent = nullptr) {
        if (other != nullptr) {
            Node<ValueType>* vertex = new Node<ValueType>;
            vertex->parent = vertex_parent;
            vertex->left = vertex_copy(other->left, iter, vertex);
            vertex->data = iter;
            ++iter;
            vertex->right = vertex_copy(other->right, iter, vertex);
            return vertex;
        }
        return nullptr;
    }

    void split(ValueType key, Node<ValueType>*& first, Node<ValueType>*& second) {
        if (lower_bound(key) != end()) {
            first = root->left;
            second = root;
            second->left = nullptr;
            if (first != nullptr) {
                first->parent = nullptr;
            }
        } else {
            first = root;
            second = nullptr;
        }
    }

    Node<ValueType>* merge(Node<ValueType>*& first, Node<ValueType>*& second) {
        if (first == nullptr) {
            return second;
        }
        if (second == nullptr) {
            return first;
        }
        Node<ValueType>* vertex = second;
        while (vertex->left != nullptr) {
            vertex = vertex->left;
        }
        splay(vertex);
        second = root;
        second->left = first;
        first->parent = second;
        return second;
    }
};
