# How to install locally

    $ cabal install --only-dependencies
    $ cabal build
    $ dist/build/guide/guide

And go to <http://localhost:8080>.

# How to install on a server

I'm going to use Digitalocean and Ubuntu, but you can use anything else.

Create a droplet with Ubuntu. Install GHC:

    $ add-apt-repository ppa:hvr/ghc
    $ apt-get update
    $ apt-get install ghc-7.10.3 cabal-install-1.22

Add cabal-install, GHC, and the cabal dir to `PATH` by adding

    /opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:$HOME/.cabal/bin

to `PATH` in `~/.bashrc` and then doing `source ~/.bashrc`.

Clone and build `guide`:

    $ git clone https://github.com/aelve/guide
    $ cd guide
    $ cabal update
    $ cabal install --dependencies-only
    $ cabal build

Delete the contents of `static/tracking.html` (because otherwise information about visits will be sent to me):

    $ truncate -s 0 static/tracking.html

Make a new subdomain in Apache. For me, it means writing this to `/etc/apache2/sites-available/guide.conf`:

~~~
<VirtualHost *:80>
  ServerName guide.aelve.com

  ProxyPreserveHost On
  ProxyPass / http://0.0.0.0:8080/
  ProxyPassReverse / http://0.0.0.0:8080/
</VirtualHost>
~~~

Enable the site:

    $ a2ensite guide
    $ service apache2 reload

Create a daemon. This goes to `/etc/init/guide.conf` (the path is going to be something other than `/root/guide` for you):

~~~
start on filesystem or runlevel [2345]
stop on runlevel [!2345]
chdir /root/guide
env LC_ALL=en_US.UTF-8
exec dist/build/guide/guide
~~~

And start the daemon:

    $ service guide start
