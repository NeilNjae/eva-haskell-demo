# eva-demo

Code to start conversations about [Haskell](https://wiki.haskell.org/Haskell). 

[Learn you a Haskell](http://learnyouahaskell.com/chapters), [Introduction to Haskell 98](https://www.haskell.org/tutorial/index.html), and [Hackage](https://hackage.haskell.org/) are good resources.

The [Stack documentation](https://docs.haskellstack.org/en/stable/README/) and [How I Start: Haskell](http://howistart.org/posts/haskell/1/) are good sources of using the tools. 

# Toolchain

I'm using the basic Haskell Platform installation, together with `stack` to manage the packages and dependencies (install with
```
$ sudo aptitude install haskell-platform haskell-stack
```
), then updgrade with
```
 stack upgrade --binary-only
```
as the version in the Ubuntu repos is too old to work with current Haskell Stack package sets.

## Creating the repository and project
Create the repository as normal: create the project in Gitolite, clone it, and insert the `.gitignore` and `README.md` files.

There's just one package, with the code in the `src` directories of the root directory. 

Create the basic `stack` project. This will create a new directory. Note that this new directory name can't have a hyphen-delimited word that's just digits, so the project will have to be `advent-of-code`

```
stack new eva-demo --bare simple
```

Modify the `stack.yaml` file as needed, such as adding the `ghc-options` stanza. 

## Creating packages

Each package lives in a separate directory, with its own `package.yaml` file and code in the `src` directory. (I based this configuration from [mstksg's setup](https://github.com/mstksg/advent-of-code-2018).)

Compile with
```
stack build
```
or 
```
stack build main
```

Run with
```
stack exec main
```

If you want to pass in additional RTS parameters, do it like this:
```
stack exec -- main +RTS -K0 -RTS
```

Run interactively with
```
stack ghci main
```
or 
```
stack ghci main:exe:main
```
if the first form is ambiguous. 

To profile, use 
```
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" advent01
```
then run with
```
stack exec --profile -- advent01 +RTS -p -hy
```
Generate the profile graph with
```
stack exec hp2ps advent01.hp
```

# Packages

Stack is using the [lts-16.10 resolver](https://www.stackage.org/lts-16.10) for packages, so make sure you read the [correct documentation for the packages included in it](https://www.stackage.org/lts-16.10/docs).

