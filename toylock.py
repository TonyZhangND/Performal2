from trace import *

class Message:
    def __init__(self, src, dst, ep):
        self.src = src
        self.dst = dst
        self.epoch = ep


class Network:
    def __init__(self):
        self.mgs = dict()   # map from dst to msg

class Node_State(object):
    def __init__(self, id, epoch, locked, n):
        self.id = id
        self.epoch = epoch
        self.locked = locked
        self.n = n  # system size

    def copy(self):
        return Node_State(self.id, self.epoch, self.locked, self.n)


class Global_State(object):
    def __init__(self, n):
        assert n > 0
        self.nodes = [Node_State(i, 0, False) for i in range(n)]
        self.msgs = Network()

        # Give the first node the lock
        self.nodes[0].epoch == 1
        self.nodes[0].locked = True


class Action(object):
    def __init__(self, actor: int):
        self.actor = actor   # which node performed this action?
        self.msg = msg       # what is the message triggering this action?

class Grant(Action): 
    def enabled(self, state: Node_State, msgs: Network):
        return state.locked == True

    def apply(self, pred: Node_State) -> (Node_State, Message):
        return (Node_State(pred.id, pred.epoch, False), 
                Message(pred.id, (pred.id + 1) % pred.n))


class Accept(Action):
    def enabled(self, state: Node_State, msgs: Network):
        return (state.locked == False 
                and state.id in msgs 
                and msgs[state.id].epoch > state.epoch)

    def apply(self, pred: Node_State) -> (Node_State, Message):
        return (Node_State(pred.id, self.msg.epoch, True), 
                    None)
