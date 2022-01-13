import React, { Component } from 'react';
import { Link } from "react-router-dom";
import classnames from 'classnames';
import { makeRoutePath } from '../lib/util';

class SidebarItem extends Component {
  render() {
    const { props } = this;

    let selectedClass = (props.selected)
    ? "bg-gray5 bg-gray1-d"
    : "pointer hover-bg-gray5 hover-bg-gray1-d";

    return (
      <Link to={makeRoutePath(props.what, true)} key="what">
        <div className={"w-100 v-mid f9 ph4 z1 pv1 " + selectedClass}>
          <p className="f9 dib">{props.what}</p>
        </div>
      </Link>
    );
  }
}

export class Skeleton extends Component {
  render() {
    const { props } = this;

    let items = [
      'apps',
      'spider',
      'ames',
      'behn',
      //TODO 'clay',
      'eyre'
    ];
    items = items.map(what => {
      return (<SidebarItem what={what} selected={props.selected === what}/>);
    });

    let rightPanelHide = this.props.rightPanelHide
      ? "dn-s" : "";

    const status = props.status
      ? (<div style={{
            position: 'absolute', right: '16px', bottom: '16px',
            padding: '8px', border: '1px solid #e22'
          }}>
          {props.status}
        </div>)
      : null;

    return (
      <div className="absolute h-100 w-100 mono">
        <div className="cf w-100 h-100 flex">
          <div className="bn br-m br-l br-xl b--gray4 b--gray1-d lh-copy h-100 flex-shrink-0 mw5-m mw5-l mw5-xl pt3 pt0-m pt0-l pt0-xl relative">
            <a className="db dn-m dn-l dn-xl f8 pb3 pl3" href="/">⟵ EScape</a>
            <div className="overflow-y-scroll h-100">
              <div className="w-100 bg-transparent">
                <Link
                  className="dib f9 pointer green2 gray4-d pa4"
                  to={"/~chat/join/~/~dopzod/urbit-help"}>
                  Get help
                </Link>
              </div>
              {items}
            </div>
          </div>
          {status}
          <div className={"h-100 w-100 flex-auto overflow-scroll relative " + rightPanelHide} style={{
            flexGrow: 1,
            padding: '8px'
          }}>
            {this.props.children}
          </div>
        </div>
      </div>
    );
  }
}
