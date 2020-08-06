import React, { Component } from 'react';

import { Link } from 'react-router-dom';
import { InviteSearch, Invites } from '../../../components/InviteSearch';
import { Spinner } from '../../../components/Spinner';
import { Toggle } from '../../../components/toggle';
import { RouteComponentProps } from 'react-router-dom';
import { Groups, GroupPolicy, Resource } from '../../../types/group-update';
import { Contacts, Rolodex } from '../../../types/contact-update';
import GlobalApi from '../../../api/global';
import { Patp, PatpNoSig, Enc } from '../../../types/noun';

type NewScreenProps = Pick<RouteComponentProps, 'history'> & {
  groups: Groups;
  contacts: Rolodex;
  api: GlobalApi;
};

type TextChange = React.ChangeEvent<HTMLTextAreaElement | HTMLInputElement>;
type BooleanChange = React.ChangeEvent<HTMLInputElement>;


interface NewScreenState {
  groupName: string;
  title: string;
  description: string;
  invites: Invites;
  privacy: boolean;
  groupNameError: boolean;
  awaiting: boolean;
}

export class NewScreen extends Component<NewScreenProps, NewScreenState> {
  constructor(props) {
    super(props);

    this.state = {
      groupName: '',
      title: '',
      description: '',
      invites: { ships: [], groups: [] },
      privacy: false,
      // color: '',
      groupNameError: false,
      awaiting: false,
    };

    this.groupNameChange = this.groupNameChange.bind(this);
    this.descriptionChange = this.descriptionChange.bind(this);
    this.invChange = this.invChange.bind(this);
    this.groupPrivacyChange = this.groupPrivacyChange.bind(this);
  }

  groupNameChange(event: TextChange) {
    const asciiSafe = event.target.value
      .toLowerCase()
      .replace(/[^a-z0-9~_.-]/g, '-');
    this.setState({
      groupName: asciiSafe,
      title: event.target.value,
    });
  }

  descriptionChange(event: TextChange) {
    this.setState({ description: event.target.value });
  }

  invChange(value: Invites) {
    this.setState({
      invites: value,
    });
  }

  groupPrivacyChange(event: BooleanChange) {
    this.setState({
      privacy: event.target.checked,
    });
  }

  onClickCreate() {
    const { props, state } = this;

    if (!state.groupName) {
      this.setState({
        groupNameError: true,
      });
      return;
    }

    const aud = state.invites.ships.map((ship) => `~${ship}`);

    const policy: Enc<GroupPolicy> = state.privacy
      ? {
          invite: {
            pending: aud,
          },
        }
      : {
          open: {
            banRanks: [],
            banned: [],
          },
        };

    const { groupName } = this.state;
    this.setState(
      {
        invites: { ships: [], groups: [] },
        awaiting: true,
      },
      () => {
        props.api.contacts
          .create(groupName, policy, this.state.title, this.state.description)
          .then(() => {
            this.setState({ awaiting: false });
            props.history.push(
              `/~groups/ship/~${window.ship}/${state.groupName}`
            );
          });
      }
    );
  }

  render() {
    let groupNameErrElem = <span />;
    if (this.state.groupNameError) {
      groupNameErrElem = (
        <span className='f9 inter red2 ml3 mt1 db'>
          Group must have a name.
        </span>
      );
    }

    return (
      <div className='h-100 w-100 mw6 pa3 pt4 overflow-x-hidden bg-gray0-d white-d flex flex-column'>
        <div className='w-100 dn-m dn-l dn-xl inter pt1 pb6 f8'>
          <Link to='/~groups/'>{'⟵ All Groups'}</Link>
        </div>
        <div className='w-100 mb4 pr6 pr0-l pr0-xl'>
          <h2 className='f8'>Create New Group</h2>
          <h2 className='f8 pt6'>Group Name</h2>
          <textarea
            className={
              'f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
              'focus-b--black focus-b--white-d'
            }
            rows={1}
            placeholder='Jazz Maximalists Research Unit'
            style={{
              resize: 'none',
              height: 48,
              paddingTop: 14,
            }}
            onChange={this.groupNameChange}
          />
          {groupNameErrElem}
          <h2 className='f8 pt6'>
            Description <span className='gray2'>(Optional)</span>
          </h2>
          <textarea
            className={
              'f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 mt2 ' +
              'focus-b--black focus-b--white-d'
            }
            rows={1}
            placeholder='Two trumpeters and a microphone'
            style={{
              resize: 'none',
              height: 48,
              paddingTop: 14,
            }}
            onChange={this.descriptionChange}
          />
          <div className='mv7'>
            <Toggle
            boolean={this.state.privacy}
            change={this.groupPrivacyChange}
            />
            <span className='dib f9 white-d inter ml3'>Private Group</span>
            <p className='f9 gray2 pt1' style={{ paddingLeft: 40 }}>
              If private, new members must be invited
            </p>
          </div>
          {this.state.privacy && (
            <>
              <h2 className='f8 pt6'>
                Invite <span className='gray2'>(Optional)</span>
              </h2>
              <p className='f9 gray2 lh-copy'>
                Selected ships will be invited to your group
              </p>
              <div className='relative pb6 mt2'>
                <InviteSearch
                  groups={{}}
                  contacts={this.props.contacts}
                  groupResults={false}
                  shipResults={true}
                  invites={this.state.invites}
                  setInvite={this.invChange}
                />
              </div>
            </>
          )}
          <button
            onClick={this.onClickCreate.bind(this)}
            className='f9 ba pa2 b--green2 green2 pointer bg-transparent'
          >
            Start Group
          </button>
          <Link to='/~groups'>
            <button className='f9 ml3 ba pa2 b--black pointer bg-transparent b--white-d white-d'>
              Cancel
            </button>
          </Link>
          <Spinner
            awaiting={this.state.awaiting}
            classes='mt4'
            text='Creating group...'
          />
        </div>
      </div>
    );
  }
}
