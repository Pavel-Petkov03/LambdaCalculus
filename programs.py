from abc import ABC, abstractmethod


class LambdaTerm(ABC):
    @abstractmethod
    def substitute(self, var, replacement):
        raise NotImplemented("Not implemented for base class")

    @abstractmethod
    def free_variables(self) -> set[str]:
        raise NotImplemented("Not implemented for base class")


class Variable(LambdaTerm):
    def __init__(self, name: str):
        self.name = name

    def __repr__(self):
        return self.name

    def substitute(self, var, replacement):
        if self.name == var.name:
            return replacement
        return self

    def free_variables(self) -> set[str]:
        return set(self.name)  # FV({X}) = {X}


class Application(LambdaTerm):
    def __init__(self, f: LambdaTerm, argument: LambdaTerm):
        self.function = f
        self.argument = argument

    def __repr__(self):
        return f"{self.function}({self.argument})"

    def substitute(self, var, replacement):
        # M1M2[x->N] = (M1[x->N])(M2[x -> N])
        return Application(
            self.function.substitute(var, replacement),
            self.argument.substitute(var, replacement)
        )

    def free_variables(self) -> set[str]:
        # FV(MN) = FV(M) union FV(N)
        return self.function.free_variables() | self.argument.free_variables()


class Abstraction(LambdaTerm):
    def __init__(self, var: Variable, body: LambdaTerm):
        self.body = body
        self.bound_var = var

    def __repr__(self):
        return f"\\{self.bound_var.name} -> {self.body}"

    def substitute(self, var, replacement):
        if self.bound_var.name == var.name:  # (lambda x : P)[x -> N] = lambda x : P
            return self
        elif self.bound_var.name not in replacement.free_variables() \
                and var.name not in self.body.free_variables():
            # (lambda x : P)[x -> N] := lambda y : P[x -> N]
            # when y!= x and x is not in FV(P) and y is not in FV(N)
            return Abstraction(self.bound_var, self.body.substitute(var, replacement))
        else:
            # (lambda y : P)[x -> N] := lambda z : P[y-> z][x->N] only when z is not in FV(P) and  FV(N)
            # the relation is functional because I take fresh variable within the scope
            # todo make the algorythm create infinite amount of variable names
            forbidden_variables = replacement.free_variables() | self.body.free_variables()
            new_variable_name = self.get_fresh_variable_name(forbidden_variables)
            variable = Variable(new_variable_name)
            new_body = self.body.substitute(self.bound_var, variable)
            return Abstraction(variable, new_body).substitute(var, replacement)

    def free_variables(self) -> set[str]:
        # FV(lambda x: M) =  FV(M) \ {x}
        return self.body.free_variables() - {self.bound_var.name}

    @staticmethod
    def get_fresh_variable_name(forbidden_variables: set[str]):
        start_variable_code = 97
        while True:
            current_letter = chr(start_variable_code)
            if current_letter not in forbidden_variables:
                return current_letter
            start_variable_code += 1


class S(Abstraction):
    def __init__(self):
        super().__init__(
            Variable("x"),
            Abstraction(Variable("y"),
                        Abstraction(Variable("z"),
                                    Application(
                                        Application(Variable("x"), Variable("z")),
                                        Application(Variable("y"), Variable("z"))
                                    )
                                    )
                        )
        )

    def __repr__(self):
        return "S"


class K(Abstraction):
    def __init__(self):
        super().__init__(
            Variable("x"),
            Abstraction(Variable("y"),
                        Variable("x")
                        )
        )

    def __repr__(self):
        return "K"


class ToCombinatorEncoder:
    K = K()
    S = S()

    def encode(self, term: LambdaTerm):
        # there is better way but will refactor later
        if isinstance(term, Abstraction) and isinstance(term.body, Abstraction) \
                and not isinstance(term.body, S) and not isinstance(term.body, K):

            # from inner to outer to destroy inner lambdas first
            return self.encode(
                Abstraction(term.bound_var, self.encode(term.body))
            )
        if isinstance(term, Abstraction) and \
                isinstance(term.body, Variable) \
                and term.body.name == term.bound_var.name:
            return Application(Application(self.S, self.K), self.K)
        elif isinstance(term, Abstraction) and \
                term.bound_var.name not in term.body.free_variables():
            return Application(self.K, term.body)
        elif isinstance(term, Abstraction) and isinstance(term.body, Application):
            return Application(
                Application(self.S,
                            self.encode(Abstraction(term.bound_var,
                                                    term.body.function
                                                    )
                                        )),
                self.encode(Abstraction(term.bound_var,
                                        term.body.argument))
            )
        return term


current = Abstraction(
    Variable("x"),
    Abstraction(Variable("y"),
                Application(Variable("x"), Variable("y"))
                )
)

comb = ToCombinatorEncoder()
print(comb.encode(current))
