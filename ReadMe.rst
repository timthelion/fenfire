Fenfire
=======

- www.fenfire.org is a cursor-based RDF nav/edit.
- Fenfire v0.2 source <http:./README>.
  v0.1+ is a 2007 rewrite in Haskell.
  Original 2003 Fenfire was Java and Python. [#]_
- `2013 resurrection by ~timthelion <RESURECTION-NOTES.md>`_
- 2016 Dockerization (Debian/wheezy-packports):

  .. image:: screenshots/docker-wheezy-fenfire_0.2-ghc_7.4.1-cabal_1.14.0.png

  After getting `~timthelion GIT`__, build was fairly trivial.

  Added an example RDF.
  TODO: Fenfire has a presentation mode, would be nice to get a demo file.

1. Build, run::

    docker-host $ docker build -t fenfire .
    docker-host $ docker run -ti -P --name fenfire fenfire

2. Setup SSH root user::

    docker-host $ docker cp .../.ssh/id_?sa.pub \
          container:/root/.ssh/authorized_keys

3. View example RDF::

    x11-host $ ssh -X user@docker-container -p SSH_PORT
    container $ /root/bin/fenfire /root/elvisimp.rdf



.. __: https://github.com/timthelion/fenfire

.. [#] http://savannah.gnu.org/cgi-bin/viewcvs/fenfire/

- 2016 `subuserization <http://subuser.org>`_

1. Install and run::

    $ subuser subuser add fenfire fenfire@https://github.com/timthelion/fenfire.git
    $ subuser run fenfire

2. Develop::

    $ git clone https://github.com/timthelion/fenfire.git
    $ cd fenfire
    $ subuser dev fenfire
    $ cabal configure --user
    
You can now open up another terminal and edit the source code. When you want to build, go back to the ``dev`` terminal and type::

    $ cabal build ; cabal install
    $ ~/.cabal/bin/fenfire

If you end up having to edit the Dockerfile and want to update your dev environment simply exit out of the dev environment and run::

    $ subuser dev fenfire --update
