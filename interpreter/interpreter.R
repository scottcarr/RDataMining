#library(compiler)
source("~/r-release-branch/R/src/library/compiler/R/cmp.r")

varBindings <- list()

append.list <- function(l, item) {
    l[[length(l)+1]] = item
    l
}

pop <- function(stack) {
    #print(stack)
    if (length(stack) == 0) {
        list(stack=stack, item=NULL)
    } else {
        last <- length(stack)
        item <- stack[[last]]
        stack[last] = NULL
        list(stack=stack, item=item)
    }

}

peek <- function(stack) {
    if (length(stack) == 0) {
        NULL
    } else {
        last <- length(stack)
        stack[[last]]
    }
}

popN <- function(stack, N) {
    if (length(stack) < N) {
        list(stack=stack, item=NULL) 
    }
    items <- list()
    for (i in 1:N) {
        s <- pop(stack)
        items[[i]] = s$item
        stack = s$stack
    }
    list(stack=stack, items=items)
}

push <- function(stack, item) {
    #print(stack)
    N <- length(stack)
    stack[[N+1]] = item
    stack
}

myEval <- function(dsm) {
    bytes <- dsm[[2]]
    symbols <- dsm[[3]]
    stack <- list()
    i <- 1
    while (i < length(bytes)+1) {
        #print(typeof(bytes[[i]]))
        type <- typeof(bytes[[i]])
        if (type == "symbol") {
            print(stack)
            op <- bytes[[i]]
            #print(op)
            if (op == "RETURN.OP") {
                print("RETURN.OP")
                #s <- pop(stack)
                #return(s$item)
                break
            }
            ind <- which(names(Opcodes.argc) == op)
            argc <- Opcodes.argc[[ind]]
            #print(argc)
            if (argc > 0) {
                #print(argc)
                for (j in 1:argc) {
                    stack = push(stack, bytes[[i+j]])
                }
            }
            stack = evalOp(op, stack, symbols)
            #print(stack)
        }
        i = i + 1
    }
    #print(stack)
    s <- pop(stack)
    return(s$item)

}

evalOp <- function(op, stack, symbols) {
    #print(paste0("Evaling: ", op))
    #print(stack)
    switch(as.character(op),
        "LDCONST.OP" = {
            s = pop(stack)
            stack <- s$stack
            arg <- s$item
            print(paste0("LDCONST ", symbols[[arg+1]]))
            push(stack, symbols[[arg+1]])
        },
        "PUSHCONSTARG.OP" = {
            s = pop(stack)
            stack <- s$stack
            ind <- s$item
            const <- symbols[[ind+1]]
            print(paste0("PUSHCONSTARG ", const))
            last <- length(stack)
            if (is.list(stack[[last]])) {
                s = pop(stack)
                stack = s$stack
                item = s$item
                item2 <- append.list(item, const)
                push(stack, item2) 
            } else {
                push(stack, list(const))
            }
            
        },
        "GETFUN.OP" = {
            s = pop(stack)
            stack <- s$stack
            arg <- s$item
            symb <- symbols[[arg+1]]
            print(paste0("GETFUN ", symb))
            print(varBindings)
            binding <- varBindings[[as.character(symb)]]
            if (is.null(binding)) {
                push(stack, get(as.character(symb)))
            } else {
                push(stack, binding)
            }
        },
        "CALL.OP" = {
            s = popN(stack,3)
            stack <- s$stack
            fun <- s$items[[3]]
            args <- s$items[[2]]
            call <- s$items[[1]]
            print("CALL.OP")
            print(fun)
            print(args)
            print(call)
            push(stack, do.call(fun, args))
        },
        "MAKECLOSURE.OP" = {
            s = pop(stack)
            stack <- s$stack
            arg <- s$item
            clos <- symbols[[arg+1]]
            #formalArgs <- clos[[1]]
            #byteCode <- clos[[2]]
            func <- as.function(unlist(clos))
            print("MAKECLOSURE.OP")
            print(func)
            push(stack, func)
        },
        "SETVAR.OP" = {
            s = pop(stack)
            stack <- s$stack
            arg <- s$item
            symb <- symbols[[arg+1]]
            print("SETVAR.OP")
            print(symb)
            print("<-")
            print(peek(stack))
            varBindings[[as.character(symb)]] <<- peek(stack)
            print(varBindings)
            stack
        },
        "INVISIBLE.OP" = {
            # what does this do?
            print("INVISIBLE.OP")
            stack
        },
        "POP.OP" = {
            print("POP.OP")
            pop(stack)$stack
        }
    )
}


test <- function(e, optimize=0) {
    bc <- compile(e, options=list(optimize=optimize))
    d <- disassemble(bc)
    res1 <- myEval(d)
    res2 <- eval(bc)
    print("---")
    if (!identical(res1, res2)) {
        print("test failed.")
        print("R")
        print(res2)
        print("mine")
        print(res1)
        FALSE
    } else {
        print("test passed")
        TRUE
    }
}

regressionTests <- function() {
    es <- c (
        quote(1+2),
        quote(c(1,2,3)),
        quote(
            { 
                x <- function(a,b) { return (a + b) }
                x(1,2) 
            }
        )
    )
    for (i in 1:length(es)) {
        if (!test(es[i])) {
            print("test failed:")
            print("optimize=0")
            print(es[i])
            return (FALSE)
        }
    }
}
#e <- quote(1+2)
#e <- quote(c(1,2,3))
#e <- quote(
#    { x <- function(a,b) { return (a + b) }
#    x(1,2) })
#e <- quote(
#    x <- function(a,b) { return (a + b) })
#bc <- compile(e, options=list(optimize=3))
#d <- disassemble(bc)
#print(d[[1]])
#print(eval(bc))
#test(e, optimize=0)
#test(e, optimize=3)
regressionTests()

