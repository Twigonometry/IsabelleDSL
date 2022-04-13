class Calculator:
    def add(self, x):
        self.result = self.result + int(x)
        return self

    def sub(self, x):
        self.result = self.result - int(x)
        return self

    def mul(self, x):
        self.result = self.result * int(x)
        return self

    def div(self, x):
        self.result = self.result / int(x)
        return self

    def clear(self):
        self.result = 0
        return self

    def getResult(self):
        return self.result
    
    def __init__(self):
        self.input = 0

c = Calculator()
print(c.clear().getResult())
print(c.add(5).sub(4).div(4).getResult())
