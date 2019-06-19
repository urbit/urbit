import React, { Component } from 'react';
import { BrowserRouter, Route } from 'react-router-dom';
import classnames from 'classnames';
import { Link } from 'react-router-dom';
import { PublishCreate } from '/components/lib/publish-create';
import { HeaderMenu } from '/components/lib/header-menu';
import { PathControl } from '/components/lib/path-control';
import { Switch, withRouter } from 'react-router';

export class Header extends Component {
  constructor(props){
    super(props);
  }

  render() {
    return (
      <div className="cf w-100 bg-white h-publish-header">
        <Switch>
          <Route exact path="/~publish/recent" component={HeaderMenu}/>
          <Route exact path="/~publish/subs" component={HeaderMenu}/>
          <Route exact path="/~publish/pubs" component={HeaderMenu}/>

          <Route exact path="/~publish/new" 
            render={ (props) => {
              return (
                <PathControl {...this.props} {...props}/>
              )
            }}/>
          <Route exact path="/~publish/new/blog"
            render={ (props) => {
              return (
                <PathControl {...this.props} {...props}/>
              )
            }}/>
          <Route exact path="/~publish/new/post"
            render={ (props) => {
              return (
                <PathControl {...this.props} {...props}/>
              )
            }}/>

          <Route exact path="/~publish/:ship/:blog"
            render={ (props) => {
              return (
                <PathControl {...this.props} {...props}/>
              )
            }}/>
          <Route exact path="/~publish/:ship/:blog/:post"
            render={ (props) => {
              return (
                <PathControl {...this.props} {...props}/>
              )
            }}/>
        </Switch>
      </div>
    );
  }
}
