struct tuple_0;
struct closure_4;
struct tuple_0 {
};
enum closure_4_tag {
  closure_4_id,
};
union closure_4_value {
  struct tuple_0 id;
};
struct closure_4 {
  enum closure_4_tag tag;
  union closure_4_value value;
};
int call_closure_4(int x, struct closure_4 var_1);
int id(int x);
int id(int x);
int main();
int call_closure_4(int x, struct closure_4 var_1) {
  int var_2;;
  switch (var_1.tag) {
    case closure_4_id: {
      struct tuple_0 var_3 = var_1.value.id;
      int var_4 = id(x);
      var_2 = var_4;
    }
  }
  return var_2;
}
int id(int x) {
  return x;
}
int id(int x) {
  return x;
}
int main() {
  struct tuple_0 var_9 = (struct tuple_0) {};
  struct closure_4 var_8 = (struct closure_4) { closure_4_id, { .id = var_9 } };
  int var_10 = 3;
  int var_7 = call_closure_4(var_10, var_8);
  return var_7;
}
