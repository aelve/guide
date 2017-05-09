import React, { Component } from 'react';

class Search extends Component {

    render() {

        return (
            <div>
            <form action="/haskell">
                <input type="text" name="q" id="search" placeholder="search" value=""></input>
            </form>
            <style jsx>{`
              #search {
                font-size: 200%;
                font-weight: 200;
                border: 1px solid #aaa;
                border-radius: 3px;
                padding: 3px 10px;
                width: 100%;
              }
            `}</style>
            </div>
        )
    }
}

module.exports = { Search }