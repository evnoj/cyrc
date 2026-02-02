# stacked pane system
- stacked panes are implemented by a margins node with no margins set, which contains nested bar nodes for each pane in the stack that isn't at the front, and then a borders node with a pane in it that is the front pane of the stack
- DIDN'T WORK: each bar and the front pane should have a parameter indicating that it is in a stack, and the margin container node should have a parameter that is a list of the node IDs of the panes in the stack
- no way to store custom parameters or fields on margin and bar layout nodes
- will need to find a way with parameters on panes
- idea: each stack is represented as a parameter in the :client scope. It is a struct of stacks, where the key is a unique id for the stack, and the value is a struct with fields:
  - path: the layout path from the root of the layout to the margin container
  - panes: the ordered list of panes in the stack
- each pane has a parameter that states the stacks it is in
- when a pane is removed, it gets removed from any stacks that it is in

implementation:
- [x] func that given the list of panes in the stack renders a margin node containing the stack, or if the list is just one pane it renders a bordered pane
- [x] func to add a stacked pane
- [ ] func that given a layout node path returns the path to the margin container from the root of the layout
- [x] action to rotate panes in stack fwd/back (bind to u/o)
- [x] action to arbitrarily reorder panes, maybe via text editor
  - implemented rotate fwd/back actions that don't affect the pane at the front of the stack, allowing the front pane to be moved anywhere in the stack
  - would still like a simpler "rearrange a list" style interface
- [ ] customize the built in actions that alter the layout to check 
- [ ] custom remove pane action that checks if it is in a stack and re-render it if so
- [ ] custom split actions that check if in a stack, and if so make the split outside the stack

thoughts:
- Tracking the path to every pane as a parameter on the client is a pain, because it means every action that modifies the layout needs to check if it's affecting any of these paths. Do I actually need to track the path to every stack? Maybe could just do with the "stack id" parameter on the pane in the front of the stack. Then, to get the path to the stack, just go up until I hit a margin node. Then it's only actions that operate on a specific pane that need to check if that pane represents the front of the stack.

limitations:
- not supporting nested stacks
- not supporting splits in stacks
- a stack only has panes
