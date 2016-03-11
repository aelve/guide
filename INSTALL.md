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

## Database

If the `state/` directory doesn't exist, it will be created. However, you can get the current state of [guide.aelve.com](http://guide.aelve.com):

    $ git clone https://github.com/aelve/guide-database
    $ mv guide-database state

You can set automatic backups to your own repository, too.

Create `.gitignore` in the `state/` folder:

~~~
events*
*.lock
~~~

Create a repository locally and remotely. If you're using Github, you can avoid having to enter passwords by generate an access token and using it as username when adding a remote:

    $ git remote add origin https://<token>@github.com/aelve/guide-database.git

Next, create `upload.sh`:

~~~
cd /root/guide/state
git add -A
GIT_COMMITTER_NAME='auto' GIT_COMMITTER_EMAIL='' git commit --author="auto <>" -m "`date`"
git push
~~~

Finally, make a cron job that would try to upload new data every 10m (tho the actual checkpoints are only created once per hour):

    $ crontab -e

~~~
*/10 * * * * /root/guide/upload.sh
~~~
