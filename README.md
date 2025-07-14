# AnuLang
---

Virtual Machine based programming language priorotizing simplicity and speed
with syntax that's mostly similar to Lua

## Features of AnuLang (so far)

- Control flow
- Functions
- Variables and expressions
- While loops
- Printing to stdout
- comments
- multiple data-types

## Syntax

```lua

# here is a basic sum function
func sum(x,y)
    return x + y
end

# functions use the 'func' prefix
func main()
    # variables can be of multiple data-types
    i = 0 # numbers
    name = "anu" # strings
    is_cool = true # booleans

    # comments use the '#' prefix
    # we have if statements like these
    # notice how AnuLang uses != unlike Lua which uses ~=
    if sum(5,5) != 0 then
        return
    end

    # AnuLang doesn't care about how you write it at all
    # so you could also write it like this
    if 10 != 10 then return end

    # unlike lua, you don't need to use 'local' prefix to define locals
    # variables defined outside functions are globals and inside scopes are locals
    value = 1

    # here is an example for a while loop
    while value != 11 do
        # print isn't a function but an instruction, so you don't use parenthesis
        print value
        value = value + 1
    end
end
```