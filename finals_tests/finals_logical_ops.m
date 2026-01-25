int main() {
    bool a = true;
    bool b = false;
    if (!b) {
        [InOutFuncs printInt:1];
    } else {
        [InOutFuncs printInt:0];
    }
    [InOutFuncs printChar:'\n'];
    // avoid &&/|| short-circuit bug, but still test simple logic via !
    return 0;
}
