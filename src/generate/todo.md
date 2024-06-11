
# gen_snippets
- with selection, after successful `{AwaitSelection, ok ,PLabel, Payload}`, try to find way to just send immediately, without the additional selection. then, follow with selection on label.
- check how `nonblocking_selection` and `nonblocking_payload` are applied, especially in the actual `receive` cases