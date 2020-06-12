import React, { Component } from 'react';
import { deSig, uxToHex } from '/lib/util';
import { Route, Link } from "react-router-dom";

import { LoadingScreen } from './loading';
import { Spinner } from './lib/icons/icon-spinner';
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
      color: "",
      disabled: false,
      type: "Editing"
    };

    this.changeTitle = this.changeTitle.bind(this);
    this.changeDescription = this.changeDescription.bind(this);
    this.changeColor = this.changeColor.bind(this);
    this.submitColor = this.submitColor.bind(this);
    this.renderDelete = this.renderDelete.bind(this);
    this.renderMetadataSettings = this.renderMetadataSettings.bind(this);
    this.markAllAsSeen = this.markAllAsSeen.bind(this);
  }

  componentDidMount() {
    if ((this.props.resource) && ("metadata" in this.props.resource)) {
      this.setState({
        title: this.props.resource.metadata.title,
        description: this.props.resource.metadata.description,
        color: `#${uxToHex(this.props.resource.metadata.color || '0x0')}`
      });
    }
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;

    if (!!state.isLoading && !props.resource) {
      this.setState({
        isLoading: false
      }, () => {
        props.history.push('/~link');
      });
    }

    if (((props.resource) && ("metadata" in props.resource))
      && (prevProps !== props)) {
      this.setState({
        title: props.resource.metadata.title,
        description: props.resource.metadata.description,
        color: `#${uxToHex(this.props.resource.metadata.color || '0x0')}`
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

  submitColor() {
    const { props, state } = this;
    const { resource } = props;

    if (!("metadata" in resource)) {
      resource.metadata = {};
    }

    //submit color if valid
    let color = state.color;
    if (color.startsWith("#")) {
      color = state.color.substr(1);
    }
    let hexExp = /([0-9A-Fa-f]{6})/
    let hexTest = hexExp.exec(color);
    let currentColor = "000000";
    if (props.resource && "metadata" in props.resource) {
      currentColor = uxToHex(props.resource.metadata.color);
    }
    if (hexTest && (hexTest[1] !== currentColor)) {
      if (props.amOwner) {
        this.setState({disabled: true});
        api.metadataAdd(
          props.resourcePath,
          props.groupPath,
          resource.metadata.title,
          resource.metadata.description,
          resource.metadata['date-created'],
          color
        ).then(() => {
          this.setState({disabled: false});
        });
      }
    }
  }

  removeCollection() {
    const { props, state } = this;

    this.setState({
      isLoading: true,
      disabled: true,
      type: "Removing"
    });
    api.removeCollection(props.resourcePath)
    .then(() => {
      this.setState({
        isLoading: false
      });
    });
  }

  deleteCollection() {
    const { props, state } = this;

    this.setState({
      isLoading: true,
      disabled: true,
      type: "Deleting"
    });
    api.deleteCollection(props.resourcePath)
    .then(() => {
      this.setState({
        isLoading: false
      });
    });
  }

  markAllAsSeen() {
    api.seenLink(this.props.resourcePath);
  }

  renderRemove() {
    const { props, state } = this;

    return (
      <div className="w-100 fl mt3">
        <p className="f8 mt3 lh-copy db">Remove Collection</p>
        <p className="f9 gray2 db mb4">
          Remove this collection from your collection list.
        </p>
        <a onClick={this.removeCollection.bind(this)}
           className="dib f9 black gray4-d bg-gray0-d ba pa2 b--black b--gray1-d pointer">
          Remove collection
        </a>
      </div>
    );
  }

  renderDelete() {
    const { props, state } = this;

    if (!props.amOwner) {
      return null;
    } else {
      return (
        <div className="w-100 fl mt3">
          <p className="f8 mt3 lh-copy db">Delete Collection</p>
          <p className="f9 gray2 db mb4">
            Delete this collection, for you and all group members.
          </p>
          <a onClick={this.deleteCollection.bind(this)}
             className="dib f9 ba pa2 b--red2 red2 pointer bg-gray0-d">
            Delete collection
          </a>
        </div>
      );
    }
  }

  renderMetadataSettings() {
    const { props, state } = this;
    const { resource } = props;

    if (!("metadata" in resource)) {
      resource.metadata = {};
    }

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
            disabled={!props.amOwner || this.state.disabled}
            onChange={this.changeTitle}
            onBlur={() => {
              if (props.amOwner) {
                this.setState({ disabled: true });
                api.metadataAdd(
                  props.resourcePath,
                  props.groupPath,
                  state.title,
                  resource.metadata.description,
                  resource.metadata['date-created'],
                  uxToHex(resource.metadata.color)
                ).then(() => {
                  this.setState({ disabled: false });
                });
              }
            }}
          />
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
              disabled={!props.amOwner || this.state.disabled}
              onChange={this.changeDescription}
              onBlur={() => {
                if (props.amOwner) {
                  this.setState({ disabled: true });
                  api.metadataAdd(
                    props.resourcePath,
                    props.groupPath,
                    resource.metadata.title,
                    state.description,
                    resource.metadata['date-created'],
                    uxToHex(resource.metadata.color)
                  ).then(() => {
                    this.setState({ disabled: false });
                  });
                }
              }}
            />
          </div>
          <p className="f8 mt3 lh-copy">Change color</p>
          <p className="f9 gray2 db mb4">Give this collection a color when viewing group channels</p>
          <div className="relative w-100 flex"
            style={{ maxWidth: "10rem" }}>
            <div className="absolute"
              style={{
                height: 16,
                width: 16,
                backgroundColor: state.color,
                top: 13,
                left: 11
              }} />
            <input
              className={"pl7 f8 ba b--gray3 b--gray2-d bg-gray0-d white-d " +
                "focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3"}
              value={this.state.color}
              disabled={!props.amOwner || this.state.disabled}
              onChange={this.changeColor}
              onBlur={this.submitColor}
            />
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
                className="dib f9 fw4 lh-solid v-top"
                style={{ width: "max-content" }}>
                {props.resource.metadata.title}
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
              className="dib f9 fw4 lh-solid v-top"
              style={{ width: "max-content" }}>
              {props.resource.metadata.title}
            </h2>
          </Link>
          <LinksTabBar {...props}/>
        </div>
        <div className="w-100 pl3 mt4 cf">
          <h2 className="f8 pb2">Collection Settings</h2>
          <p className="f8 mt3 lh-copy db">Mark all links as read</p>
          <p className="f9 gray2 db mb4">Mark all links in this collection as read.</p>
          <a className="dib f9 black gray4-d bg-gray0-d ba pa2 b--black b--gray1-d pointer"
            onClick={this.markAllAsSeen}>
              Mark all as read
            </a>
          {this.renderRemove()}
          {this.renderDelete()}
          {this.renderMetadataSettings()}
          <Spinner
            awaiting={this.state.disabled}
            classes="absolute right-1 bottom-1 pa2 ba b--black b--gray0-d white-d"
            text={`${this.state.type} collection...`}
          />
        </div>
      </div>
    );
  }
}