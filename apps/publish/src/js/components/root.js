import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import Mousetrap from 'mousetrap';
import classnames from 'classnames';
import _ from 'lodash';

import { api } from '/api';
import { store } from '/store';
import { Recent } from '/components/recent';
import { Header } from '/components/header';
import { Blog } from '/components/blog';
import { Post } from '/components/post';
import { HeaderBar } from '/components/lib/header-bar';

export class Root extends Component {
  constructor(props) {
    super(props);
    this.state = store.state;

    console.log("root.state", this.state);

    store.setStateHandler(this.setState.bind(this));
  }

  render() {
    return (
      <div className="fl w-100">
        <HeaderBar />
        <BrowserRouter>
          <Header {...this.state} />
          <Route exact path="/~publish/recent"
            render={ (props) => {
              return (
                <div className="fl w-100">
                  <Recent
                    {...this.state}
                  />
                </div>
              );
           }} />
          <Route exact path="/~publish/subs"
            render={ (props) => {
              return (
                <div className="fl w-100">
                  <Recent
                    {...this.state}
                  />
                </div>
              );
           }} />
          <Route exact path="/~publish/pubs"
            render={ (props) => {
              return (
                <div className="fl w-100">
                  <Recent
                    {...this.state}
                  />
                </div>
              );
           }} />

          <Route exact path="/~publish/:ship/:blog"
            render={ (props) => {
              return (
                <div className="fl w-100">
                  <Blog
                    blogId = {props.match.params.blog}
                    ship = {props.match.params.ship.slice(1)}
                    {...this.state}
                  />
                </div>
              );
           }} />

          <Route exact path="/~publish/:ship/:blog/:post"
            render={ (props) => {
              return (
                <div className="fl w-100">
                  <Post
                    blogId = {props.match.params.blog}
                    postId = {props.match.params.post}
                    ship = {props.match.params.ship.slice(1)}
                    {...this.state}
                  />
                </div>
              );
           }} />

        </BrowserRouter>
      </div>
    );
  }
}

