import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import classnames from 'classnames';
import _ from 'lodash';
import { HeaderBar } from './lib/header-bar';
import { Popout } from './lib/icons/popout';
import { History } from './history';
import { Input } from './input';
import { api } from '../api';
import { store } from '../store';

export class Root extends Component {
  constructor(props) {
    super(props);
    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  componentDidMount() {
    //preload spinner asset
    new Image().src = "/~dojo/img/Spinner.png";
  }

  render() {
    return (
      <BrowserRouter>
        <div className="w-100 h-100 bg-white bg-gray1-d">
        <HeaderBar/>
          <Route
            exact path="/~dojo/:popout?"
            render={(props) => {
              let popout = !!props.match.params.popout;

              let popoutClasses = classnames({
                "h-100-m40-p1-ns": !popout,
                "h-100-m h-100-l h-100-xl": popout,
                "mh4-m mh4-l mh4-xl": !popout,
                "mb4-m mb4-l mb4-xl": !popout,
                "ba-m ba-l ba-xl": !popout
              })

              return (
                <div className="w-100 h-100 flex-m flex-l flex-xl">
                  <div
                    className="db dn-m dn-l dn-xl inter bg-white bg-gray0-d dt w-100"
                    style={{ height: 40 }}>
                    <a className="f8 pl3 black white-d dtc h-100 v-mid" href="/">
                      ⟵ Landscape
                    </a>
                  </div>
                  <div className={"pa3 bg-white bg-gray0-d black white-d mono w-100 f8 relative" +
                  " h-100-m40-s b--gray2 br1 flex-auto " + popoutClasses}
                    style={{
                      lineHeight: "1.4",
                      cursor: "text"
                    }}>
                    <Popout popout={popout}/>
                    <History commandLog={this.state.txt} />
                    <Input
                      ship={ship}
                      cursor={this.state.cursor}
                      prompt={this.state.prompt}
                      input={this.state.input}
                    />
                  </div>
                </div>
              );
            }}
          />
        </div>
      </BrowserRouter>
    );
  }
}
