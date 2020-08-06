import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { Spinner } from '../../../../components/Spinner';
import { Toggle } from '../../../../components/toggle';
import { GroupView } from '../../../../components/Group';
import { deSig, uxToHex, writeText } from '../../../../lib/util';
import { roleForShip, resourceFromPath } from '../../../../lib/group';

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
    this.changePolicy = this.changePolicy.bind(this);
  }

  componentDidMount() {
    const { props } = this;
    if (props.association.metadata) {
      this.setState({
        title: props.association.metadata.title,
        description: props.association.metadata.description
      });
    }
  }

  componentDidUpdate(prevProps) {
    const { props } = this;
    if (prevProps !== this.props) {
      if (props.association.metadata) {
        this.setState({
          title: props.association.metadata.title,
          description: props.association.metadata.description
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

  changePolicy() {
    this.setState({ awaiting: true }, () => {
      this.props.api.groups.changePolicy(resourceFromPath(this.props.path),
      Boolean(this.props.group?.policy?.open)
        ? { replace: { invite: { pending: [] } } }
        : { replace: { open: { banned: [], banRanks: [] } } }
        ).then(() => this.setState({ awaiting: false }));
      }
      );
    }

  renderDetail() {
    const { props } = this;

    const responsiveClass =
      props.activeDrawer === 'detail' ? 'db ' : 'dn db-ns ';

    let channelList = [];

    Object.keys(props.associations).filter((app) => {
      return app !== 'contacts';
    }).map((app) => {
      Object.keys(props.associations[app]).filter((channel) => {
        return props.associations[app][channel]['group-path'] === props.association['group-path'];
      })
      .map((channel) => {
        const channelObj = props.associations[app][channel];
        const title =
        channelObj.metadata?.title || channelObj['app-path'] || '';

        const color = uxToHex(channelObj.metadata?.color) || '000000';
        const channelPath = channelObj['app-path'];
        const link = `/~${app}/join${channelPath}`;
        return(
          channelList.push({
            title: title,
            color: color,
            app: app.charAt(0).toUpperCase() + app.slice(1),
            link: link
          })
        );
      });
    });

    const isEmpty = (Boolean(channelList.length === 0));

    if (channelList.length === 0) {
      channelList = <div />;
    } else {
      channelList = channelList.sort((a, b) => {
        return a.title.toLowerCase().localeCompare(b.title.toLowerCase());
      }).map((each) => {
        const overlay = {
          r: parseInt(each.color.slice(0, 2), 16),
          g: parseInt(each.color.slice(2, 4), 16),
          b: parseInt(each.color.slice(4, 6), 16)
        };

        return (
          <li key={each.link} className="f9 list flex pv1 w-100">
            <div className="ba" style={{
              borderColor: `#${each.color}`,
              backgroundColor: `rgba(${overlay.r}, ${overlay.g}, ${overlay.b}, 0.25)`
            }}
            >
              <img
                src={`/~landscape/img/${each.app}.png`}
                className="dib invert-d pa1 v-mid"
                style={{ height: 26, width: 26 }}
              />
            </div>
              <div className="flex flex-column flex-auto">
                <p className="f9 inter ml2 w-100">{each.title}</p>
                <p className="f9 inter mt2 ml2 w-100">
                  <span className="f9 di mr2 inter">{each.app}</span>
                  <Link className="f9 di green2" to={each.link}>
                    Open
              </Link>
                </p>
              </div>
            </li>
        );
      });
    }

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
    if (props.association?.metadata) {
      title = (props.association.metadata.title !== '')
        ? props.association.metadata.title
        : props.path.substr(1);
      description = (props.association.metadata.description !== '')
        ? props.association.metadata.description
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
            {props.group.members.size + ' participant' +
             ((props.group.members.size === 1) ? '' : 's')}
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

    const { group, association } = props;

    const ourRole = roleForShip(group, window.ship);
    const groupOwner = (ourRole === 'admin');

    const deleteButtonClasses = (groupOwner) ? 'b--red2 red2 pointer bg-gray0-d' : 'b--gray3 gray3 bg-gray0-d c-default';

    const tags = [
      { description: 'Admin', tag: 'admin', addDescription: 'Make Admin' },
      { description: 'Moderator', tag: 'moderator', addDescription: 'Make Moderator' },
      { description: 'Janitor', tag: 'janitor', addDescription: 'Make Janitor' }
    ];

    let shortcode = <div />;

    if (group?.policy?.open) {
      shortcode = <div className="mt4">
        <p className="f9 mt4 lh-copy">Share</p>
        <p className="f9 gray2 mb2">Share a shortcode to join this group</p>
        <div className="relative w-100 flex"
          style={{ maxWidth: '29rem' }}
        >
          <input
            className="f8 mono ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 flex-auto mr3"
            disabled={true}
            value={props.path.substr(6)}
          />
          <span className="lh-solid f8 pointer absolute pa3 inter"
            style={{ right: 12, top: 1 }}
            ref="copy"
            onClick={() => {
              writeText(props.path.substr(6));
              this.refs.copy.innerText = 'Copied';
            }}
          >
            Copy
              </span>
        </div>
      </div>;
    }
    return (
      <div className="pa4 w-100 h-100 white-d overflow-y-auto">
        <div className="f8 f9-m f9-l f9-xl w-100">
          <Link to={'/~groups/detail' + props.path}>{'⟵ Channels'}</Link>
        </div>
        {shortcode}
      { group && <GroupView permissions className="mt6" resourcePath={props.path} group={group} tags={tags} api={props.api} /> }
        <div className={(groupOwner) ? '' : 'o-30'}>
          <p className="f9 mt3 lh-copy">Rename</p>
          <p className="f9 gray2 mb2">Change the name of this group</p>
          <div className="relative w-100 flex"
          style={{ maxWidth: '29rem' }}
          >
            <input
              className={'f9 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
              'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
              value={this.state.title}
              disabled={!groupOwner}
              onChange={this.changeTitle}
              onBlur={() => {
                if (groupOwner) {
                  this.setState({ awaiting: true }, (() => {
                    props.api.metadata.metadataAdd(
                      'contacts',
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
          <p className="f9 mt3 lh-copy">Change description</p>
          <p className="f9 gray2 mb2">Change the description of this group</p>
          <div className="relative w-100 flex"
            style={{ maxWidth: '29rem' }}
          >
            <input
              className={'f9 ba b--gray3 b--gray2-d bg-gray0-d white-d ' +
                'focus-b--black focus-b--white-d pa3 db w-100 flex-auto mr3'}
              value={this.state.description}
              disabled={!groupOwner}
              onChange={this.changeDescription}
              onBlur={() => {
                if (groupOwner) {
                  this.setState({ awaiting: true }, (() => {
                    props.api.metadata.metadataAdd(
                      'contacts',
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
          <div className="relative w-100 mt6" style={{ maxWidth: '29rem' }}>
          <Toggle
            boolean={(Boolean(group?.policy?.invite))}
            change={this.changePolicy}
          />
            <span className="dib f9 white-d inter ml3">Private Group</span>
            <p className="f9 gray2 pt1" style={{ paddingLeft: 40 }}>
              If private, members must be invited
            </p>
          </div>
          <p className="f9 mt6 lh-copy">Delete Group</p>
          <p className="f9 gray2 mb2">
          Permanently delete this group. All current members will no longer see this group.
          </p>
          <a className={'dib f9 ba pa2 ' + deleteButtonClasses}
          onClick={() => {
            if (groupOwner) {
              this.setState({ awaiting: true, type: 'Deleting' }, (() => {
                props.api.contacts.delete(props.path).then(() => {
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
