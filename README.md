This was my Master Thesis for the MSc in Computer Science at Chalmers University of Technology.

It's a natural language interface for the MusicBrainz database. You can ask it questions about music and it will fetch the info and get back at you, like a conversation.

You can get more information at my [webpage](http://diz.es/education/msc/thesis/).

### Compile and use

Use `make all` on the src folder to compile, then leave the `src/Main` daemon running and access through `web/` or the TCP port 3838.

Needs haskell, the Grammatical Framework and some extra libraries (`htx`, `curl`, `dataenc+). Of course, to use the web access you also need a web server (e.g. Apache) and PHP.

The report folder contains both the report and the slides used in the defense/presentation.

### Demo

[Try it live](http://diz.es/education/msc/thesis/demo).