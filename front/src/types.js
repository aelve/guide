
export type Category = {
  uid   : string,
  link  : string,
  title : string
  };

export type GrandCategoryT = {
  title    : string,
  finished : Array<Category>,
  wip      : Array<Category>,
  stubs    : Array<Category>
  }
