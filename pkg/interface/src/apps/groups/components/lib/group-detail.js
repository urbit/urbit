import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { Spinner } from '../../../../components/Spinner';
import { deSig, uxToHex } from '../../../../lib/util';

export class GroupDetail extends Component {
  constructor(props) {
    super(props);
    this.state = {
      title: '',
      description: '',
      awaiting: false,
      type: 'Editing'
    };
    this.changeTitle = this.changeTitle.bind(this);
    this.changeDescription = this.changeDescription.bind(this);
  }

  componentDidMount() {
    const { props } = this;
    const channelPath = `${props.path}/contacts${props.path}`;
    if ((props.association) && (props.association[channelPath])) {
      this.setState({
        title: props.association[channelPath].metadata.title,
        description: props.association[channelPath].metadata.description
      });
    }
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (prevProps !== this.props) {
      const channelPath = `${props.path}/contacts${props.path}`;
      if ((props.association) && (props.association[channelPath])) {
        this.setState({
          title: props.association[channelPath].metadata.title,
          description: props.association[channelPath].metadata.description
        });
      }
    }
  }

  changeTitle(event) {
    this.setState({ title: event.target.value });
  }

  changeDescription(event) {
    this.setState({ description: event.target.value });
  }

  renderDetail() {
    const { props } = this;

    const responsiveClass =
      props.activeDrawer === 'detail' ? 'db ' : 'dn db-ns ';

    const isEmpty = (Object.keys(props.association).length === 0) ||
      ((Object.keys(props.association).length === 1) &&
        (Object.keys(props.association)[0].includes('contacts')));

    let channelList = (<div />);

    channelList = Object.keys(props.association).sort((a, b) => {
      const aChannel = props.association[a];
      const bChannel = props.association[b];

      const aTitle = aChannel.metadata.title || a;
      const bTitle = bChannel.metadata.title || b;

      return aTitle.toLowerCase().localeCompare(bTitle.toLowerCase());
    }).map((key) => {
      const channel = props.association[key];
      if (!('metadata' in channel)) {
        return <div key={channel} />;
      }

      if (channel['app-name'] === 'contacts') {
        return <div key={channel} />;
      }

      const title = channel.metadata.title || channel['app-path'] || '';
      const color = uxToHex(channel.metadata.color) || '000000';
      let app = channel['app-name'] || 'Unknown';
      const channelPath = channel['app-path'];
      const link = `/~${app}/join${channelPath}`;
      app = app.charAt(0).toUpperCase() + app.slice(1);

      const overlay = {
        r: parseInt(color.slice(0, 2), 16),
        g: parseInt(color.slice(2, 4), 16),
        b: parseInt(color.slice(4, 6), 16)
      };

      const tile = (app === 'Unknown')
        ? <div className="dib ba pa1" style={{
        backgroundColor: `#${color}`,
        borderColor: `#${color}`,
        height: 24,
        width: 24 }}
          />
        : <div className="ba" style={{
          borderColor: `#${color}`,
          backgroundColor: `rgba(${overlay.r}, ${overlay.g}, ${overlay.b}, 0.25)`
          }}
          >
            <img
            src={`/~groups/img/${app}.png`}
            className="dib invert-d pa1 v-mid"
            style={{ height: 26, width: 26 }}
            />
        </div>;

      return (
        <li key={channelPath} className="f9 list flex pv1 w-100">
          {tile}
          <div className="flex flex-column flex-auto">
            <p className="f9 inter ml2 w-100">{title}</p>
            <p className="f9 inter ml2 w-100">
              <span className="f9 di mr2 inter">{app}</span>
              <a className="f9 di green2" href={link}>
                Open
              </a>
            </p>
          </div>
        </li>
      );
    });

    let backLink = props.location.pathname;
    backLink = backLink.slice(0, props.location.pathname.indexOf('/detail'));

    const emptyGroup = (
      <div className={isEmpty ? 'dt w-100 h-100' : 'dn'}>
        <p className="gray2 f9 tc v-mid dtc">
          This group has no channels. To add a channel, invite this group using any application.
        </p>
      </div>
    );

    let title = props.path.substr(1);
    let description = '';
    const channel = `${props.path}/contacts${props.path}`;
    if ((props.association) && (props.association[channel])) {
      title = (props.association[channel].metadata.title !== '')
        ? props.association[channel].metadata.title
        : props.path.substr(1);
      description = (props.association[channel].metadata.description !== '')
        ? props.association[channel].metadata.description
        : '';
    }

    return (
      <div className={'relative h-100 w-100 bg-white bg-gray0-d white-d pa4 '
        + responsiveClass +
        ((isEmpty) ? 'overflow-hidden' : 'overflow-x-hidden')}
      >
        <div className="pb4 f8 db dn-m dn-l dn-xl">
          <Link to={backLink}>⟵ Contacts</Link>
        </div>
        <div className="w-100 lh-copy">
          <Link
            className="absolute right-1 f9"
            to={'/~groups/settings' + props.path}
          >Group Settings</Link>
          <p className="f9 mw5 mw3-m mw4-l">{title}</p>
          <p className="f9 gray2">{description}</p>
          <p className="f9">
            {props.group.size + ' participant' +
              ((props.group.size === 1) ? '' : 's')}
          </p>
        </div>
        <p className={'gray2 f9 mb2 pt6 ' + (isEmpty ? 'dn' : '')}>Group Channels</p>
        {emptyGroup}
        {channelList}
      </div>
    );
  }

  renderSettings() {
    const { props } = this;

    const groupOwner = (deSig(props.match.params.ship) === window.ship);

    const channelPath = `${props.path}/contacts${props.path}`;

    const association = ((props.association) && (props.association[channelPath]))
      ? props.association[channelPath] : {};

    const deleteButtonClasses = (groupOwner) ? 'b--red2 red2 pointer bg-gray0-d' : 'b--gray3 gray3 bg-gray0-d c-default';

    return (
      <div className="pa4 w-100 h-100 white-d">
        <div className="f8 f9-m f9-l f9-xl w-100">
          <Link to={'/~groups/detail' + props.path}>{'⟵ Channels'}</Link>
        </div>
        <div className={(groupOwner) ? '' : 'o-30'}>
          <p className="f8 mt3 lh-copy">Rename</p>
          <p className="f9 gray2 mb4">Change the name of this group</p>
          <div className="relative w-100 flex"
          style={{ maxWidth: '29rem' }}
          >
            <input
              className={'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
              'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
              value={this.state.title}
              disabled={!groupOwner}
              onChange={this.changeTitle}
              onBlur={() => {
                if (groupOwner) {
                  this.setState({ awaiting: true }, (() => {
                    props.api.metadataAdd(
                      association['app-path'],
                      association['group-path'],
                      this.state.title,
                      association.metadata.description,
                      association.metadata['date-created'],
                      uxToHex(association.metadata.color)
                    ).then(() => {
                      this.setState({ awaiting: false });
                    });
                  }));
                }
              }}
            />
          </div>
          <p className="f8 mt3 lh-copy">Change description</p>
          <p className="f9 gray2 mb4">Change the description of this group</p>
          <div className="relative w-100 flex"
            style={{ maxWidth: '29rem' }}
          >
            <input
              className={'f8 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
                'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
              value={this.state.description}
              disabled={!groupOwner}
              onChange={this.changeDescription}
              onBlur={() => {
                if (groupOwner) {
                  this.setState({ awaiting: true }, (() => {
                    props.api.metadataAdd(
                      association['app-path'],
                      association['group-path'],
                      association.metadata.title,
                      this.state.description,
                      association.metadata['date-created'],
                      uxToHex(association.metadata.color)
                    ).then(() => {
                      this.setState({ awaiting: false });
                    });
                  }));
                }
              }}
            />
          </div>
          <p className="f8 mt3 lh-copy">Delete Group</p>
          <p className="f9 gray2 mb4">
          Permanently delete this group. All current members will no longer see this group.
          </p>
          <a className={'dib f9 ba pa2 ' + deleteButtonClasses}
          onClick={() => {
            if (groupOwner) {
              this.setState({ awaiting: true, type: 'Deleting' }, (() => {
                props.api.contactView.delete(props.path).then(() => {
                  props.history.push('/~groups');
                });
              }));
            }
          }}
          >Delete this group</a>
        </div>
        <Spinner awaiting={this.state.awaiting} text={`${this.state.type} group...`} classes="pa2 ba absolute right-1 bottom-1 b--gray1-d" />
      </div>
    );
  }

  render() {
    const render = (this.props.settings)
      ? this.renderSettings() : this.renderDetail();

    return render;
  }
}

export default GroupDetail;
