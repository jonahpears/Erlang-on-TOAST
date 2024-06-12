
# gen_snippets
- with selection, after successful `{AwaitSelection, ok ,PLabel, Payload}`, try to find way to just send immediately, without the additional selection. then, follow with selection on label.
- check how `nonblocking_selection` and `nonblocking_payload` are applied, especially in the actual `receive` cases
-[ ] fix numbering of data within scopes

# monitor spec extraction
-[x] update to move timers into timeouts, and treat them similarly.

# monitors 
-[x] update to handle timers/timeouts.
-[ ] update with QoL presets for runtime enforcement (migrate from old version, v9)

# toast -> input mapping
-[ ] handle timing constraints
