#library(compiler)
source("~/r-release-branch/R/src/library/compiler/R/cmp.r")

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
            op <- bytes[[i]]
            #print(op)
            if (op == "RETURN.OP") {
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
            push(stack, get(as.character(symb)))
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
        }

    )
}



bc <- compile(quote(1+2), options=list(optimize=0))
#bc <- compile(quote(1+2))
d <- disassemble(bc)
res <- myEval(d)
print("---")
print(res)

