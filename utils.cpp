#include <unordered_set>
#include <string>
#include <vector>

std::unordered_set<std::string> class_names = {
    "NSString",
    "NSNumber",
    "NSArray",
    "NSMutableArray"
};

void add_class_name(const char* name) {
    if (name) {
        class_names.insert(name);
    }
}

int is_class_name(const char* name) {
    if (!name) return 0;
    return class_names.find(name) != class_names.end() ? 1 : 0;
}

const char** get_all_class_names(int* count) {
    static std::vector<const char*> names;
    static std::vector<std::string> strings;
    
    names.clear();
    strings.clear();
    
    for (const auto& name : class_names) {
        strings.push_back(name);
        names.push_back(strings.back().c_str());
    }
    
    if (count) *count = names.size();
    return names.data();
}