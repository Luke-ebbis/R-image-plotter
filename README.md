# The `pictuR` programme

The `pictuR` programme plots an pdf with images and allows for the optional creation of a description from a `csv` file. If you want a description, put a `csv` file in the zip that has a name and a description colunm. Example of the zip:
```
figures.zip/
├── description.csv
├── P1070164 (Klein).JPG
├── P1070165 (Klein).JPG
├── P1070166 (Klein).JPG
├── P1070167 (Klein).JPG
├── P1070168 (Klein).JPG
├── P1070169 (Klein).JPG
├── P1070170 (Klein).JPG
...                   ├─ the extension
└── P1070239 (Klein).JPG
    ^^^^^   ^^^^^^^^
      |       └─ remove pattern
      └─ Name pattern
```
If the remove and name pattern are not specified, then the pdf will just contain the file names. If they are specified (as is default in the programme), then only the last 3 numbers will be printed.