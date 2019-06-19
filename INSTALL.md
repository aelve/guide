# Points to keep in mind

The `state/` directory contains the database. You can download the current database of [guide.aelve.com](http://guide.aelve.com) by doing this:

    $ git clone https://github.com/aelve/guide-database
    $ mv guide-database state
    $ (cd state; zstd -d *.zst)

The `config.json` file contains the config (it will be created at the 1st start). There are 5 settings so far:

  * `admin-password` is the password for the admin panel (at `/admin`). Leave it empty if you don't want any password.

  * `google-token` lets you set the Google site verification token. Leave it empty if you don't want Google verification.

  * `base-url` is the URL of the server (which should contain `http://` or `https://`). It's used for feed generation.

  * `prerender` enables prerendering pages when the server starts. By default it's disabled (because it's annoying during development).

  * `discuss-url` adds a “discuss this site” link under the header. You can leave it as `null`.

# How to install locally

    $ stack build
    $ stack exec guide

And go to <http://localhost:8080>. The status page is available at <http://localhost:5050>; the admin page, at <http://localhost:8080/admin>.

# How to install on a server

I'm going to use Digitalocean and Ubuntu, but you can use anything else.

Create a droplet with Ubuntu. Install Stack (this command will import a GPG key, add Stack's repository, and run `apt-get`):

    $ curl -sSL https://get.haskellstack.org/ | sh

Clone and build `guide`:

    $ git clone https://github.com/aelve/guide
    $ cd guide
    $ stack build --no-test --no-bench --copy-bins

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

Enable the `remoteip` module (this is needed so that the `/admin` page would display actual IPs instead of `127.0.0.1`):

    $ a2enmod remoteip

Enable the site:

    $ a2ensite guide
    $ service apache2 reload

Create a daemon. This goes to `/etc/init/guide.conf` (the path is going to be something other than `/root/guide` for you):

~~~
start on filesystem or runlevel [2345]
stop on runlevel [!2345]
chdir /root/guide
env LC_ALL=en_US.UTF-8
exec /root/.local/bin/guide
~~~

Start the daemon:

    $ service guide start

## Database

You can set automatic backups of the database to your own repository.

Create `.gitignore` in the `state/` folder:

~~~
Archive/
events*
*.lock
*.log
~~~

Create a repository locally and remotely. If you're using Github, you can avoid having to enter passwords by generate an access token and using it as username when adding a remote:

    $ git remote add origin https://<token>@github.com/aelve/guide-database.git

Next, create `upload.sh`:

~~~
cd /root/guide/state
rm -f *.zst
zstd -f *.log
git add -A
GIT_COMMITTER_NAME='auto' GIT_COMMITTER_EMAIL='' git commit --author="auto <>" -m "`date`"
git push
~~~

    $ chmod +x upload.sh

Finally, make a cron job that would try to upload new data every 10m (though the actual checkpoints are only created once per six hours):

    $ crontab -e

~~~
*/10 * * * * /bin/bash /root/guide/upload.sh
~~~
