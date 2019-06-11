import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import Mousetrap from 'mousetrap';
import classnames from 'classnames';
import _ from 'lodash';

import { api } from '/api';
import { store } from '/store';
import { Recent } from '/components/recent';
import { Header } from '/components/header';
import { New } from '/components/new';
import { NewBlog } from '/components/new-blog';
import { NewPost } from '/components/new-post';
import { Skeleton } from '/components/skeleton';
import { Blog } from '/components/blog';
import { Post } from '/components/post';
import { HeaderBar } from '/components/lib/header-bar';
import { Switch } from 'react-router';

export class Root extends Component {
  constructor(props) {
    super(props);
    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));
  }

  render() {
    return (
      <BrowserRouter>
        <Switch>
          <Route exact path="/~publish/recent"
            render={ (props) => {
              return (
                <Skeleton
                  children={
                    <Recent {...this.state} />
                  }
                />
              );
           }} />
          <Route exact path="/~publish/subs"
            render={ (props) => {
              return (
                <Skeleton
                  children={
                    <Recent {...this.state} />
                  }
                />
              );
           }} />
          <Route exact path="/~publish/pubs"
            render={ (props) => {
              return (
                <Skeleton
                  children={
                    <Recent {...this.state} />
                  }
                />
              );
           }} />

          <Route exact path="/~publish/new"
            render={ (props) => {
              return (
                <Skeleton
                  {...this.state}
                  children={
                    <New api={api} {...this.state} {...props}/>
                  }
                />
              );
           }} />

          <Route exact path="/~publish/new/blog"
            render={ (props) => {
              return (
                <Skeleton
                  {...this.state}
                  children={
                    <NewBlog api={api} {...this.state} {...props}/>
                  }
                />
              );
           }} />

          <Route exact path="/~publish/new/post"
            render={ (props) => {
              return (
                <Skeleton
                  {...this.state}
                  children={
                    <NewPost api={api} {...this.state} {...props}/>
                  }
                />
              );
           }} />

          <Route exact path="/~publish/:ship/:blog"
            render={ (props) => {
              return (
                <Skeleton
                  {...this.state}
                  children={
                    <Blog
                      blogId = {props.match.params.blog}
                      ship = {props.match.params.ship.slice(1)}
                      {...this.state}
                    />
                  }
                />
              );
           }} />

          <Route exact path="/~publish/:ship/:blog/:post"
            render={ (props) => {
              return (
                <Skeleton
                  {...this.state}
                  children={
                    <Post
                      blogId = {props.match.params.blog}
                      postId = {props.match.params.post}
                      ship = {props.match.params.ship.slice(1)}
                      {...this.state}
                    />
                  }
                />
              );
           }} />
        </Switch>
      </BrowserRouter>
    );
  }
}
