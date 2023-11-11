// Columns contain lists of values
type Column = {
  name: string;
  values: unknown[];
};

// A table is a non-empty list of columns
type Table = [Column, ...Column[]];

// `UserTable` is a subtype of `Table`:
type UserTable = [
  { name: "firstName"; values: string[] },
  { name: "age"; values: [1, 2, 3] },
  { name: "isAdmin"; values: boolean[] }
];

type GetColumn<TableT extends Table, ColumnN extends string> = TableT extends [
  infer FirstColumn extends Column,
  ...infer OtherColumns extends Table
]
  ? FirstColumn extends { name: ColumnN; values: infer Values } // Pattern matching
    ? Values
    : GetColumn<OtherColumns, ColumnN>
  : never;

type Boo = GetColumn<UserTable, "age">;
