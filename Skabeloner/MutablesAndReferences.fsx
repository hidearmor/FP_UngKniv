//  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
//  °°°°°°°°°°°°°°   MUTABLES   °°°°°°°°°°°°°°°°
//  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

// F# has some imperative features
// One of them is the possibility to create a mutable variables
// Here's an example:
    let mutable x = 1;;

// What happens is that the a 'location' is created where the value
// 1 is stored. x is then a variable pointing to that location. 
// We can update the value stored in the location, by the following syntax:
    x <- 5;;

// Records can have mutable fields (H.R 180 - 182)
// The following is a declaration of a type intRec which is record with a mutable field:
    type intRec  = { mutable count : int };;    

// We can declare a function that increments the count field, using the mutable syntax from above:
    let incr (x: intRec) =
        x.count <- x.count + 1
        x.count;;

//  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
//  °°°°°°°°°°°°°°   REFERENCES   °°°°°°°°°°°°°°
//  °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

// The 'ref' type provides a shorthand for a record with a mutable field (see H.R 182-183 for definitions)
// This might seem puzzling at first, but try to think of it this way: 
// If a variable is of type 'int list ref', the variable itself is not a int list,
// but actually a record, with a mutable field og type 'int list'.
// In order to retrieve the value referenced by a variable (x) of type 'a' ref', we use the following syntax:
    !x;;
// In order to update a value of a mutable field referenced by a variable (x), we use the following syntax:
    x := [3; 4];;

// The following code takes an 'int ref' is input, increments the value that is being referenced, and returns the value:
// (Note the sequential composition used here, which is the ";" - further readings found here H.R 179)
    let incr1 r = (r := !r + 1 ; !r);;


// Reference types were also used in the exam2022 Question 2:
    type Node<'a> = Root of 'a
                | Link of 'a * Node<'a> ref

// and subquestion 2.2 uses the !-syntax in pattern matching:
    let rec find (e: Node<'a> ref) =
        match !e with
        | Root a -> e
        | Link (a, e') -> find e'

// NOTE! It is recommended to use x.Value instead of !x, to avoid misinterpretations between programmers
// (! also being used as negation in many languages)