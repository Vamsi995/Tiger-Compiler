# Alisetti Sai Vamsi -  111801002

## Build

To make the parser execute ```make all``` in the home directory.
To clean the generated files use ```make clean``` in the home directory.


## Usage

The parser prints the AST of the given input file using the ```printast.sml``` file. 

```
$ make all 
$ cd tiger
$ ./tiger < <filename>.tig
```

### Using the pretty printer

```
$ make tc
$ cd src/
$ ./tc <filename>.tig

```

## Running the test cases


```
$ make all 
$ cd tiger
$ ./tiger < ../Tests/<filename>.tig
```

