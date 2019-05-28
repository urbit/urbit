import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";

import { store } from '/store';
import Home from '/components/home';

export default class App extends Component {

  constructor() {
    super();
    this.state = {
    };

    store.setStateHandler(this.setState.bind(this));
  }

  render() {
    return (
       <BrowserRouter>
         <div>
          <Route exact path="/~launch" 
            render={ (props) => {
              return (
                <Home 
                  {...props}
                  data={this.state} 
                  keys={new Set(Object.keys(this.state))}
                />
              );
            }}
          />
        </div>
      </BrowserRouter>
    );
  }
}

window.app = App;
