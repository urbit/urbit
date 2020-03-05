import React, { Component } from 'react';
import classnames from 'classnames';
import { deSig, uxToHex } from '/lib/util';
import { Route, Link } from "react-router-dom";

import { LoadingScreen } from './loading';
import { LinksTabBar } from '/components/lib/links-tabbar';
import SidebarSwitcher from './lib/icons/icon-sidebar-switch';
import { makeRoutePath } from '../lib/util';

export class SettingsScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      isLoading: false,
      title: "",
      description: "",
      color: ""
    };

    this.changeTitle = this.changeTitle.bind(this);
    this.changeDescription = this.changeDescription.bind(this);
    this.changeColor = this.changeColor.bind(this);
    this.renderDelete = this.renderDelete.bind(this);
    this.renderMetadataSettings = this.renderMetadataSettings.bind(this);
  }

  componentDidMount() {
    if (this.props.resource) {
      this.setState({
        title: this.props.resource.title,
        description: this.props.resource.description,
        color: uxToHex(this.props.resource.color || '0x0')
      });
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;
    if (!!state.isLoading && !props.resource) {
      this.setState({
        isLoading: false
      }, () => {
        api.setSpinner(false);
        props.history.push('/~link');
      });
    }

    if (props.resource && (prevProps !== props)) {
      this.setState({
        title: props.resource.title,
        description: props.resource.description,
        color: uxToHex(props.resource.color || '0x0')
      });
    }
  }

  changeTitle() {
    this.setState({title: event.target.value})
  }

  changeDescription() {
    this.setState({description: event.target.value});
  }

  changeColor() {
    this.setState({color: event.target.value});
  }

  deleteCollection() {
    const { props, state } = this;

    api.deleteCollection(props.resourcePath);
    api.setSpinner(true);

    this.setState({
      isLoading: true
    });
  }

  renderDelete() {
    const { props, state } = this;

    const isManaged = ('/~/' !== props.groupPath.slice(0,3));

    let deleteButtonClasses = (props.amOwner) ? 'b--red2 red2 pointer bg-gray0-d' : 'b--grey3 grey3 bg-gray0-d c-default';
    let leaveButtonClasses = (!props.amOwner) ? "pointer" : "c-default";

    let deleteClasses = 'dib f9 black gray4-d bg-gray0-d ba pa2 b--black b--gray0-d pointer';
    let deleteText = 'Remove this collection from your collection list.';
    let deleteAction = 'Remove';
    if (props.amOwner && isManaged) {
      deleteText = 'Delete this collection. (All group members will no longer see this chat.)';
      deleteAction = 'Delete';
      deleteClasses = 'dib f9 ba pa2 b--red2 red2 pointer bg-gray0-d';
    }

    return (
      <div className="w-100 fl mt3">
        <p className="f8 mt3 lh-copy db">Delete Collection</p>
        <p className="f9 gray2 db mb4">{deleteText}</p>
        <a onClick={this.deleteCollection.bind(this)}
           className={deleteClasses}>{deleteAction + ' collection'}</a>
      </div>
    );
  }

  renderMetadataSettings() {
    const { props, state } = this;
    const { resource } = props;

    return(
      <div>
        <div className={"w-100 pb6 fl mt3 " + ((props.amOwner) ? '' : 'o-30')}>
        <p className="f8 mt3 lh-copy">Rename</p>
        <p className="f9 gray2 db mb4">Change the name of this collection</p>
        <div className="relative w-100 flex"
        style={{maxWidth: "29rem"}}>
          <input
            className={"f8 ba b--gray3 b--gray2-d bg-gray0-d white-d " +
            "focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3"}
            value={this.state.title}
            disabled={!props.amOwner}
            onChange={this.changeTitle}
          />
          <span className={"f8 absolute pa3 inter " +
          ((props.amOwner) ? "pointer" : "")}
            style={{ right: 12, top: 1 }}
            ref="rename"
            onClick={() => {
              if (props.amOwner) {
                api.setSpinner(true);
                api.metadataAdd(
                  props.resourcePath,
                  props.groupPath,
                  state.title,
                  props.resource.description,
                  props.resource['date-created'],
                  uxToHex(props.resource.color)
                ).then(() => {
                  api.setSpinner(false);
                  this.refs.rename.innerText = "Saved";
                });
              }
            }}>
            Save
            </span>
          </div>
          <p className="f8 mt3 lh-copy">Change description</p>
          <p className="f9 gray2 db mb4">
            Change the description of this collection
          </p>
          <div className="relative w-100 flex"
            style={{ maxWidth: "29rem" }}>
            <input
              className={"f8 ba b--gray3 b--gray2-d bg-gray0-d white-d " +
                "focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3"}
              value={this.state.description}
              disabled={!props.amOwner}
              onChange={this.changeDescription}
            />
            <span className={"f8 absolute pa3 inter " +
              ((props.amOwner) ? "pointer" : "")}
              style={{ right: 12, top: 1 }}
              ref="description"
              onClick={() => {
                if (props.amOwner) {
                  api.setSpinner(true);
                  api.metadataAdd(
                    props.resourcePath,
                    props.groupPath,
                    props.resource.title,
                    state.description,
                    props.resource['date-created'],
                    uxToHex(props.resource.color)
                  ).then(() => {
                    api.setSpinner(false);
                    this.refs.description.innerText = "Saved";
                  });
                }
              }}>
              Save
            </span>
          </div>
          <p className="f8 mt3 lh-copy">Change color</p>
          <p className="f9 gray2 db mb4">Give this collection a color when viewing group channels</p>
          <div className="relative w-100 flex"
            style={{ maxWidth: "20rem" }}>
            <input
              className={"f8 ba b--gray3 b--gray2-d bg-gray0-d white-d " +
                "focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3"}
              value={this.state.color}
              disabled={!props.amOwner}
              onChange={this.changeColor}
            />
            <span className={"f8 absolute pa3 inter " +
              ((props.amOwner) ? "pointer" : "")}
              style={{ right: 12, top: 1 }}
              ref="color"
              onClick={() => {
                if (props.amOwner && state.color.match(/[0-9A-F]{6}/i)) {
                  api.setSpinner(true);
                  api.metadataAdd(
                    props.resourcePath,
                    props.groupPath,
                    props.resource.title,
                    props.resource.description,
                    props.resource['date-created'],
                    state.color
                  ).then(() => {
                    api.setSpinner(false);
                    this.refs.color.innerText = "Saved";
                  });
                }
              }}>
              Save
            </span>
          </div>
        </div>
      </div>
    )
  }

  render() {
    const { props, state } = this;
    const isinPopout = this.props.popout ? "popout/" : "";

    let writeGroup = Array.from(props.group.values());

    if (props.groupPath === undefined) {
      return <LoadingScreen/>;
    }

    if (!!state.isLoading) {
      return (
        <div className="h-100 w-100 overflow-x-hidden flex flex-column white-d">
          <div
            className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
            style={{ height: "1rem" }}>
            <Link to="/~link">{"⟵ All Collections"}</Link>
          </div>
          <div
            className="pl4 pt2 bb b--gray4 b--gray2-d bg-gray0-d flex relative overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0"
            style={{ height: 48 }}>
            <SidebarSwitcher
              sidebarShown={this.props.sidebarShown}
              popout={this.props.popout}
            />
            <Link to={makeRoutePath(props.resourcePath, props.popout)}
            className="pt2 white-d">
              <h2
                className="dib f9 fw4 v-top"
                style={{ width: "max-content" }}>
                {props.resource.title}
              </h2>
            </Link>
            <LinksTabBar {...props}/>
          </div>
          <div className="w-100 pl3 mt4 cf">
            <h2 className="f8 pb2">Removing...</h2>
          </div>
        </div>
      );
    }

    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column white-d">
        <div
          className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
          style={{ height: "1rem" }}>
          <Link to="/~link">{"⟵ All Collections"}</Link>
        </div>
        <div
          className="pl4 pt2 bb b--gray4 b--gray1-d flex relative overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0"
          style={{ height: 48 }}>
          <SidebarSwitcher
            sidebarShown={this.props.sidebarShown}
            popout={this.props.popout}
          />
          <Link to={makeRoutePath(props.resourcePath, props.popout)}
          className="pt2">
            <h2
              className="dib f9 fw4 v-top"
              style={{ width: "max-content" }}>
              {props.resource.title}
            </h2>
          </Link>
          <LinksTabBar {...props}/>
        </div>
        <div className="w-100 pl3 mt4 cf">
          <h2 className="f8 pb2">Collection Settings</h2>
          {this.renderDelete()}
          {this.renderMetadataSettings()}
        </div>
      </div>
    );
  }
}