Design document for chatbots.

# Introduction #

The following documents an object-oriented architecture for connecting chatbots to a Jabber server so that other Jabber users can chat to it. Currently Python and Java clients are planned, but the design is generic and should be straightforward to implement in any OOP language.

# Major Design Goals #

The chatbot client software is aimed at programmers who want to create "chatbots", and do not want to fuss with getting it on-line. Thus it is more important for this software to be easy to use and understand than it is for it to support all Jabber's features. In particular, it must be easy to use and understand for beginning programmers (i.e. students in CMPT 120/125).

This software will provide the "connection code" for putting a chatbot on-line with Jabber. The user of this code must of course have an account on a Jabber server.

In addition to connecting a chatbot to a Jabber server, a "local server" class is provided for testing and demonstration purposes. This class will let the user plug-in their chatbot so that they can chat with it locally on the same computer.

One of the provided Jabber client objects is a "human" client; by embedding this an a suitable GUI, it will result in a basic Jabber client.

Finally, transcripts of conversations are automatically logged to a text file.

# Main Classes #

## JabberServerProxyBase ##

Base clase for chatting on a Jabber server, or locally to the computer. It has two direct child classes: JabberServerProxy (for connecting to remote Jabber servers), and LocalServerProxy (for local chatting).

## JabberClientBase ##

New Jabber clients will extend this class, i.e. when a programmer wants to write a new chatbot they must fit it into a classes that extends JabberClientBase.

One child class is provided: JabberHumanClient extends JabberClientBase and is meant to be used to create chat programs for humans (like MSN).

# Python-specific Details #

The [xmppy](http://xmpppy.sourceforge.net/) library looks reasonable, although the
documentation is not great.

# Java-specific Details #

In the past, we've used the [Smack library](http://www.jivesoftware.org/smack/) for Java
support, and it still seems like a good choice.