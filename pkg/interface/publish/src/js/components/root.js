import React, { Component } from 'react';
import { BrowserRouter, Route } from "react-router-dom";
import classnames from 'classnames';
import { api } from '/api';
import { store } from '/store';
import { Recent } from '/components/recent';
import { NewBlog } from '/components/new-blog';
import { NewPost } from '/components/new-post';
import { Skeleton } from '/components/skeleton';
import { Blog } from '/components/blog';
import { Post } from '/components/post';
import { Subs } from '/components/subs';
import { Pubs } from '/components/pubs';
import { Switch } from 'react-router';

export class Root extends Component {
  constructor(props) {
    super(props);
    this.state = store.state;
    store.setStateHandler(this.setState.bind(this));

    this.setSpinner = this.setSpinner.bind(this);
  }

  setSpinner(spinner) {
    this.setState({
      spinner
    });
  }

  render() {

    return (
      <BrowserRouter>
        <Switch>
          <Route exact path="/~publish/recent"
            render={ (props) => {
              return (
                <Skeleton
                  spinner={this.state.spinner}
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
                  spinner={this.state.spinner}
                  children={
                    <Subs {...this.state} api={api}/>
                  }
                />
              );
           }} />
          <Route exact path="/~publish/pubs"
            render={ (props) => {
              return (
                <Skeleton
                  spinner={this.state.spinner}
                  children={
                    <Pubs {...this.state} />
                  }
                />
              );
           }} />

          <Route exact path="/~publish/new-blog"
            render={ (props) => {
              return (
                <Skeleton
                  spinner={this.state.spinner}
                  children={
                    <NewBlog api={api}
                      {...this.state}
                      setSpinner={this.setSpinner}
                      {...props}/>
                  }
                />
              );
           }} />

          <Route exact path="/~publish/new-post"
            render={ (props) => {
              return (
                <Skeleton
                  spinner={this.state.spinner}
                  children={
                    <NewPost api={api}
                      setSpinner={this.setSpinner}
                      {...this.state}
                      {...props}/>
                  }
                />
              );
           }} />

          <Route exact path="/~publish/:ship/:blog"
            render={ (props) => {
              return (
                <Skeleton
                  spinner={this.state.spinner}
                  children={
                    <Blog
                      blogId = {props.match.params.blog}
                      ship = {props.match.params.ship.slice(1)}
                      api = {api}
                      setSpinner={this.setSpinner}
                      {...this.state}
                      {...props}
                    />
                  }
                />
              );
           }} />

          <Route exact path="/~publish/:ship/:blog/:post"
            render={ (props) => {
              return (
                <Skeleton
                  spinner={this.state.spinner}
                  children={
                    <Post
                      blogId = {props.match.params.blog}
                      postId = {props.match.params.post}
                      ship = {props.match.params.ship.slice(1)}
                      setSpinner={this.setSpinner}
                      api = {api}
                      {...this.state}
                      {...props}
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
