@interface MyPropObj : NSObject
@property NSString *name;
@end

@implementation MyPropObj
@property NSString *name;
@end

int main() {
    MyPropObj *obj = [MyPropObj new];
    [obj setName: "hi"]; 
    id s = [obj getName];
    [InOutFuncs printNSObject: s];
    [InOutFuncs printChar: '\n'];
    return 0;
}
