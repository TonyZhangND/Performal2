from toylock import *

class Root(object):
    def __init__(self, init_state: Global_State):
        self.succs = []
        self.gs = init_state


def apply_trace(init_state, trace):
    """Applies the trace of actions on the init_state to produce a final state
    Args:
        init_state (Global_State): initial state
        trace (list(Action)): list of Action objects
    """
    curr_state = init_state
    for action in trace:
        # First ensure that action is valid
        assert 0 <= action.actor < len(curr_state.nodes)
        target_node = curr_state.nodes[action.actor]
        assert action.enabled(target_node, curr_state.msgs)

        # Apply action 
        new_state, new_msg = action.apply(target_node)
        curr_state[action.actor] = new_state
        if new_msg is not None:
            curr_state.msgs[new_msg.dst] = new_msg
    return curr_state


""" Builds the execution tree """
def search(root: Root, depth: int):
    #TODO
    return None


def main():
    init_state = Global_State(3)
    root = Root(init_state)
    search(root, 30)

if __name__ == "__main__":
    # main()
    test()


def test():
    init_state = Global_State(3)

    # TODO: This is buggy. I don't have epistemic access to the message for appying the Accept
    trace = [Grant(0, None), Accept(1)]
