# RADruntime

**Example**

```bash
?- radruntime::init_sit(s0).
true.

?- view::render(s0).
Fluents
power(light,off)
door_position(closed)

Actions
turn_on(light)
open_door
true.

?- radruntime::update(open_door).
Did open_door
Fluents
power(light,off)
door_position(open)

Actions
turn_on(light)
close_door
true.

?- radruntime::update(turn_on(light)).
Did turn_on(light)
Fluents
power(light,on)
door_position(open)

Actions
turn_off(light)
close_door
true.
```
