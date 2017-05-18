// @flow

import React, { Component } from 'react';

const footerItem = (refOrObj, ...rest) => {
    switch (typeof refOrObj) {
        case 'string':
            return (<a href={refOrObj}>{rest}</a>)
        case 'object':
            return (refOrObj)
        default:
            return (<h1>Default case: {typeof refOrObj}</h1>)
    }
}

class Footer extends Component {
    render() {
        return (
            <div className="footer">
              {footerItem(<div>made by <a href="https.//artyom.me">Artyom</a></div>)}
              {footerItem(
                 <div><a href="https://github.com/aelve/guide">source</a>
                 /<a href="https://github.com/aelve/guide/issues">issue tracker</a></div>)
              }
              {footerItem("/unwritten-rules","rules")}
              {footerItem("/donate","donate")}
              {footerItem(<div>licensed under <a href="/license">CC+ BY-SA 4.0</a></div>)}      
              <style jsx>{`
                  .footer {
                      display:flex;
                      flex-flow: row wrap;
                      justify-content: space-around;
                      padding: 1.5em 0 0em 0;
                      margin: 0 -1em;
                      margin-top: 2em;
                      border-top: 1px solid black;
                      margin-bottom: 1em;
                  }

                  .footer-item {
                      display:inherit;
                      margin: 0 0.75em;
                      min-height: 40px;
                      text-align: center;
                  }
                  `}
              </style>
            </div>
        )
    }
}

module.exports = { Footer }