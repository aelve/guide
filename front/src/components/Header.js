// @flow

import React, { Component } from 'react';

class Header extends Component {
    
    render() {
        return (
          <div>
            <h1 className="header">
              <a href="/haskell">
                Aelve Guide <span>| Haskell</span>
              </a>
            </h1>
            <div className="subtitle">alpha version • don &#39;t post on Reddit yet</div>
            <style jsx>{`
              .subtitle {
                font-weight: 500;
                color: #e03;
                margin-top: 0.8em;
                margin-bottom: 2em;
              }
              
              .subtitle a {
                color: inherit;
                border-bottom: 1.5px solid;
                text-decoration: none;
              }

              .subtitle a:hover {
                color: #f35;
              }

              .header {
                font-size: 250%;
                font-weight: 600;
                margin-bottom: 0px;
              }

              .header > span {
                font-weight: 200;
              }

              .header a span {
                color: black;
                text-decoration: none;
              }

              .header a:visited {
                  color: inherit;
              }
            `}</style>
          </div>
        );
    }
}

module.exports = { Header }