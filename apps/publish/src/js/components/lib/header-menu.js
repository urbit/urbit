import React, { Component } from 'react';
import classnames from 'classnames';
import { NavLink } from 'react-router-dom';
import { PublishCreate } from '/components/lib/publish-create';
import { withRouter } from 'react-router';

const PC = withRouter(PublishCreate);

export class HeaderMenu extends Component {
  render () {
    let recentText = (this.props.unread)
      ? <p className="label-regular">
          <span className="green-medium body-large"> • </span>
          <span>Recent</span>
        </p>
      : <p className="label-regular">Recent</p>;

    let subsText = (this.props.invites)
      ? <p className="label-regular">
          <span className="green-medium body-large"> • </span>
          <span>Subscriptions</span>
        </p>
      : <p className="label-regular">Subscriptions</p>;

    return (
      <div className="fixed w-100 bg-white cf h-publish-header z-4"
        style={{top:48}}>
        <PC create={"blog"}/>
        <div className="w-100 flex">
          <div className="fl bb b-gray-30 w-16" >
          </div>

          <NavLink exact 
            className="header-menu-item"
            to="/~publish/recent"
            activeStyle={{
              color: "black",
              borderColor: "black",
            }}
            style={{flexBasis:148}}>
            Recent
          </NavLink>

          <div className="fl bb b-gray-30 w-16" >
          </div>

          <NavLink exact 
            className="header-menu-item"
            to="/~publish/subs"
            activeStyle={{
              color: "black",
              borderColor: "black",
            }}
            style={{flexBasis:148}}>
            {subsText}
          </NavLink>

          <div className="fl bb b-gray-30 w-16" >
          </div>

          <NavLink exact 
            className="header-menu-item"
            to="/~publish/pubs"
            activeStyle={{
              color: "black",
              borderColor: "black",
            }}
            style={{flexBasis:148}}>
            Notebooks
          </NavLink>

          <div className="fl bb b-gray-30 w-16" style={{flexGrow:1}}>
          </div>
        </div>
      </div>
    );
  }
}
