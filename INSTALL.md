# Points to keep in mind

The `state/` directory contains the database. You can download the current database of [guide.aelve.com](http://guide.aelve.com) by doing this:

    $ git clone https://github.com/aelve/guide-database
    $ mv guide-database state

If you want to enable tracking, replace the contents of `static/tracking.md` and set the environment variable `GUIDE_TRACKING=1` when running the server.

# How to install locally

    $ cabal install --only-dependencies
    $ cabal build
    $ dist/build/guide/guide

And go to <http://localhost:8080>. The status page is available at <http://localhost:5050>.

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

Make a new subdomain in Apache. For me, it means writing this to `/etc/apache2/sites-available/guide.conf`:

~~~
<VirtualHost *:80>
  ServerName guide.aelve.com

  ProxyPreserveHost On
  ProxyPass / http://0.0.0.0:8080/
  ProxyPassReverse / http://0.0.0.0:8080/
</VirtualHost>
~~~

If you want the status page to be available as well, write:

~~~
<VirtualHost *:80>
  ServerName guide.aelve.com

  ProxyPreserveHost On
  ProxyPass /status/ http://0.0.0.0:5050/
  ProxyPassReverse /status/ http://0.0.0.0:5050/
  ProxyPass / http://0.0.0.0:8080/
  ProxyPassReverse / http://0.0.0.0:8080/
</VirtualHost>
~~~

(Note that it will only be available at `/status/`, not `/status`.)

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

(Also possibly `env GUIDE_TRACKING=1`.)

And start the daemon:

    $ service guide start

## Database

You can set automatic backups of the database to your own repository.

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

    $ chmod +x upload.sh

Finally, make a cron job that would try to upload new data every 10m (tho the actual checkpoints are only created once per hour):

    $ crontab -e

~~~
*/10 * * * * /bin/bash /root/guide/upload.sh
~~~
