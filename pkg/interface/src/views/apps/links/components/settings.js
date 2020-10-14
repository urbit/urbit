import React, { Component } from 'react';

import { uxToHex, makeRoutePath } from '~/logic/lib/util';
import { Link } from 'react-router-dom';

import { LoadingScreen } from './loading';
import { Spinner } from '~/views/components/Spinner';
import { TabBar } from '~/views/components/chat-link-tabbar';
import SidebarSwitcher from '~/views/components/SidebarSwitch';

import { MetadataSettings } from '~/views/components/metadata/settings';

export class SettingsScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      isLoading: false,
      awaiting: false,
      type: 'Editing'
    };

    this.renderDelete = this.renderDelete.bind(this);
    this.markAllAsSeen = this.markAllAsSeen.bind(this);
    this.changeLoading = this.changeLoading.bind(this);
  }

  componentDidUpdate() {
    const { props, state } = this;

    if (Boolean(state.isLoading) && !props.resource) {
      this.setState({
        isLoading: false
      }, () => {
        props.history.push('/~link');
      });
    }
  }

  changeLoading(isLoading, awaiting, type, closure) {
    this.setState({
      isLoading,
      awaiting,
      type
    }, closure);
  }

  removeCollection() {
    const { props } = this;

    this.setState({
      isLoading: true,
      awaiting: true,
      type: 'Removing'
    });
    props.api.links.removeCollection(props.resourcePath)
    .then(() => {
      this.setState({
        isLoading: false
      });
    });
  }

  deleteCollection() {
    const { props } = this;

    this.setState({
      isLoading: true,
      awaiting: true,
      type: 'Deleting'
    });
    props.api.links.deleteCollection(props.resourcePath)
    .then(() => {
      this.setState({
        isLoading: false
      });
    });
  }

  markAllAsSeen() {
    this.props.api.links.seenLink(this.props.resourcePath);
  }

  renderRemove() {
    return (
      <div className="w-100 fl mt3">
        <p className="f8 mt3 lh-copy db">Remove Collection</p>
        <p className="f9 gray2 db mb4">
          Remove this collection from your collection list.
        </p>
        <a onClick={this.removeCollection.bind(this)}
           className="dib f9 black gray4-d bg-gray0-d ba pa2 b--black b--gray1-d pointer"
        >
          Remove collection
        </a>
      </div>
    );
  }

  renderDelete() {
    const { props } = this;

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
             className="dib f9 ba pa2 b--red2 red2 pointer bg-gray0-d mb4"
          >
            Delete collection
          </a>
        </div>
      );
    }
  }

  render() {
    const { props, state } = this;

    if (props.groupPath === undefined) {
      return <LoadingScreen />;
    }

    if (state.isLoading) {
      return (
        <div className="h-100 w-100 overflow-x-hidden flex flex-column white-d">
          <div
            className="w-100 dn-m dn-l dn-xl inter pt4 pb6 pl3 f8"
            style={{ height: '1rem' }}
          >
            <Link to="/~link">{'⟵ All Collections'}</Link>
          </div>
          <div
            className="pl4 pt2 bb b--gray4 b--gray2-d bg-gray0-d flex relative overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0"
            style={{ height: 48 }}
          >
            <SidebarSwitcher
              sidebarShown={this.props.sidebarShown}
              popout={this.props.popout}
              api={this.props.api}
            />
            <Link to={makeRoutePath(props.resourcePath, props.popout)}
            className="pt2 white-d"
            >
              <h2
                className="dib f9 fw4 lh-solid v-top"
                style={{ width: 'max-content' }}
              >
                {props.resource.metadata.title}
              </h2>
            </Link>
            <TabBar
              location={props.location}
              popout={props.popout}
              popoutHref={makeRoutePath(props.resourcePath, true, props.page)}
              settings={makeRoutePath(props.resourcePath, props.popout) + '/settings'}
            />
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
          style={{ height: '1rem' }}
        >
          <Link to="/~link">{'⟵ All Collections'}</Link>
        </div>
        <div
          className="pl4 pt2 bb b--gray4 b--gray1-d flex relative overflow-x-scroll overflow-x-auto-l overflow-x-auto-xl flex-shrink-0"
          style={{ height: 48 }}
        >
          <SidebarSwitcher
            sidebarShown={this.props.sidebarShown}
            popout={this.props.popout}
            api={this.props.api}
          />
          <Link to={makeRoutePath(props.resourcePath, props.popout)}
          className="pt2"
          >
            <h2
              className="dib f9 fw4 lh-solid v-top"
              style={{ width: 'max-content' }}
            >
              {props.resource.metadata.title}
            </h2>
          </Link>
          <TabBar
            location={props.location}
            popout={props.popout}
            popoutHref={makeRoutePath(props.resourcePath, true, props.page)}
            settings={makeRoutePath(props.resourcePath, props.popout) + '/settings'}
          />
          </div>
        <div className="w-100 pl3 mt4 cf">
          <h2 className="f8 pb2">Collection Settings</h2>
          <p className="f8 mt3 lh-copy db">Mark all links as read</p>
          <p className="f9 gray2 db mb4">Mark all links in this collection as read.</p>
          <a className="dib f9 black gray4-d bg-gray0-d ba pa2 b--black b--gray1-d pointer"
            onClick={this.markAllAsSeen}
          >
              Mark all as read
            </a>
          {this.renderRemove()}
          {this.renderDelete()}
          <MetadataSettings
            isOwner={props.amOwner}
            changeLoading={this.changeLoading}
            api={props.api}
            association={props.resource}
            resource="collection"
            app="link"
          />
          <Spinner
            awaiting={this.state.awaiting}
            classes="absolute right-1 bottom-1 pa2 ba b--black b--gray0-d white-d"
            text={this.state.type}
          />
        </div>
      </div>
    );
  }
}
