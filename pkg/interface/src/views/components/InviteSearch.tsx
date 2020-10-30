import React, { Component, createRef } from 'react';
import _ from 'lodash';
import Mousetrap from 'mousetrap';
import urbitOb from 'urbit-ob';
import { Sigil } from '~/logic/lib/sigil';
import { PatpNoSig, Path } from '~/types/noun';
import { Groups} from '~/types/group-update';
import { Rolodex, Contact } from '~/types/contact-update';
import { Associations } from '~/types/metadata-update';

export interface Invites {
  ships: PatpNoSig[];
  groups: string[][];
}

interface InviteSearchProps {
  groups: Groups;
  contacts: Rolodex;
  groupResults: boolean;
  shipResults: boolean;
  invites: Invites;
  setInvite: (inv: Invites) => void;
  disabled?: boolean;
	associations?: Associations;
}

interface InviteSearchState {
  groups: string[][];
  peers: PatpNoSig[];
  contacts: Map<PatpNoSig, string[]>;
  searchValue: string;
  searchResults: Invites;
  selected: PatpNoSig | Path | null;
  inviteError: boolean;
}

export class InviteSearch extends Component<
  InviteSearchProps,
  InviteSearchState
> {
  textarea: React.RefObject<HTMLTextAreaElement> = createRef();
  constructor(props) {
    super(props);
    this.state = {
      groups: [],
      peers: [],
      contacts: new Map(),
      searchValue: '',
      searchResults: {
        groups: [],
        ships: [],
      },
      selected: null,
      inviteError: false,
    };
    this.search = this.search.bind(this);
  }

  componentDidMount() {
    this.peerUpdate();
    this.bindShortcuts();
  }

  componentDidUpdate(prevProps) {
    if (prevProps !== this.props) {
      this.peerUpdate();
    }
  }

  peerUpdate() {
    const groups = Array.from(Object.keys(this.props.contacts))
      .filter((e) => !e.startsWith('/~/'))
      .map((e) => {
        const eachGroup: Path[] = [];
        eachGroup.push(e);
        if (this.props.associations) {
          let name = e;
          if (e in this.props.associations) {
            name =
              this.props.associations[e].metadata.title !== ''
                ? this.props.associations[e].metadata.title
                : e;
          }
          eachGroup.push(name);
        }
        return Array.from(eachGroup);
      });

    let peers: PatpNoSig[] = [];
    const peerSet = new Set<PatpNoSig>();
    const contacts = new Map();

    _.map(this.props.groups, (group, path) => {
      if (group.members.size > 0) {
        const groupEntries = group.members.values();
        for (const member of groupEntries) {
          peerSet.add(member);
        }
      }

	    const groupContacts = this.props.contacts[path];

      if (groupContacts) {
        const groupEntries = group.members.values();
        for (const member of groupEntries) {
          if (groupContacts[member]) {
            if (contacts.has(member)) {
              contacts
                .get(member)
                .push(groupContacts[member].nickname);
            } else {
              contacts.set(member, [
                groupContacts[member].nickname,
              ]);
            }
          }
        }
      }
    });
    peers = Array.from(peerSet);

    this.setState({ groups: groups, peers: peers, contacts: contacts });
  }

  search(event) {
    const searchTerm = event.target.value.toLowerCase().replace('~', '');
    const { state, props } = this;

    this.setState({ searchValue: event.target.value });

    if (searchTerm.length < 1) {
      this.setState({ searchResults: { groups: [], ships: [] } });
    }

    if (searchTerm.length > 0) {
      if (state.inviteError === true) {
        this.setState({ inviteError: false });
      }

      let groupMatches = !props.groupResults ? [] :
        state.groups.filter((e) => {
          return (
            e[0].includes(searchTerm) || e[1].toLowerCase().includes(searchTerm)
          );
        });

      let shipMatches = !props.shipResults ? []  :
         state.peers.filter((e) => {
          return (
            e.includes(searchTerm) && !props.invites.ships.includes(e)
          );
        });

      for (const contact of state.contacts.keys()) {
        const thisContact = state.contacts.get(contact) || [];
        const match = thisContact.filter((e) => {
          return e.toLowerCase().includes(searchTerm);
        });
        if (match.length > 0) {
          if (!(contact in shipMatches) && props.shipResults) {
            shipMatches.push(contact);
          }
        }
      }

      let isValid = true;
      if (!urbitOb.isValidPatp('~' + searchTerm)) {
        isValid = false;
      }

      if (props.shipResults && isValid && shipMatches.findIndex((s) => s === searchTerm) < 0) {
        shipMatches.unshift(searchTerm);
      }

      const { selected } = state;
      const groupIdx = groupMatches.findIndex(([path]) => path === selected);
      const shipIdx = shipMatches.findIndex((ship) => ship === selected);
      const staleSelection = groupIdx < 0 && shipIdx < 0;
      if (!selected || staleSelection) {
        const newSelection = _.get(groupMatches, '[0][0]') || shipMatches[0];
        this.setState({ selected: newSelection });
      }

      if (searchTerm.length < 3) {
        groupMatches = groupMatches
          .filter(([, name]) =>
            name
              .toLowerCase()
              .split(' ')
              .some((s) => s.startsWith(searchTerm))
          )
          .sort((a, b) => a[1].length - b[1].length);

        shipMatches = shipMatches.slice(0, 3);
      }

      this.setState({
        searchResults: { groups: groupMatches, ships: shipMatches },
      });
    }
  }

  bindShortcuts() {
    const mousetrap = Mousetrap(this.textarea.current);
    mousetrap.bind(['down', 'tab'], (e) => {
      e.preventDefault();
      e.stopPropagation();
      this.nextSelection();
    });

    mousetrap.bind(['up', 'shift+tab'], (e) => {
      e.preventDefault();
      e.stopPropagation();
      this.nextSelection(true);
    });

    mousetrap.bind('enter', (e) => {
      e.preventDefault();
      e.stopPropagation();
      const { selected } = this.state;
      if (selected && selected.startsWith('/')) {
        this.addGroup(selected);
      } else if (selected) {
        this.addShip(selected);
      }
      this.setState({ selected: null });
    });
  }
  nextSelection(backward = false) {
    const { selected, searchResults } = this.state;
    const { ships, groups } = searchResults;
    if (!selected) {
      return;
    }
    let groupIdx = groups.findIndex(([path]) => path === selected);
    let shipIdx = ships.findIndex((ship) => ship === selected);
    if (groupIdx >= 0) {
      backward ? groupIdx-- : groupIdx++;
      let selected = _.get(groups, [groupIdx, 0]);
      if (groupIdx === groups.length) {
        selected = ships.length === 0 ? groups[0][0] : ships[0];
      }
      if (groupIdx < 0) {
        selected =
          ships.length === 0
            ? groups[groups.length - 1][0]
            : ships[ships.length - 1];
      }
      this.setState({ selected });
      return;
    }
    if (shipIdx >= 0) {
      backward ? shipIdx-- : shipIdx++;
      let selected = ships[shipIdx];
      if (shipIdx === ships.length) {
        selected = groups.length === 0 ? ships[0] : groups[0][0];
      }

      if (shipIdx < 0) {
        selected =
          groups.length === 0
            ? ships[ships.length - 1]
            : groups[groups.length - 1][0];
      }

      this.setState({ selected });
    }
  }
  deleteGroup() {
    const { ships } = this.props.invites;
    this.setState({
      searchValue: '',
      searchResults: { groups: [], ships: [] },
    });
    this.props.setInvite({ groups: [], ships: ships });
  }

  deleteShip(ship) {
    let { groups, ships } = this.props.invites;
    this.setState({
      searchValue: '',
      searchResults: { groups: [], ships: [] },
    });
    ships = ships.filter((e) => {
      return e !== ship;
    });
    this.props.setInvite({ groups: groups, ships: ships });
  }

  addGroup(group) {
    this.setState({
      searchValue: '',
      searchResults: { groups: [], ships: [] },
    });
    this.props.setInvite({ groups: [group], ships: [] });
  }

  addShip(ship) {
    const { groups, ships } = this.props.invites;
    this.setState({
      searchValue: '',
      searchResults: { groups: [], ships: [] },
    });
    if (!ships.includes(ship)) {
      ships.push(ship);
    }
    if (groups.length > 0) {
      return false;
    }
    this.props.setInvite({ groups: groups, ships: ships });
    return true;
  }

  submitShipToAdd(ship) {
    const searchTerm = ship.toLowerCase().replace('~', '').trim();
    let isValid = true;
    if (!urbitOb.isValidPatp('~' + searchTerm)) {
      isValid = false;
    }
    if (!isValid) {
      this.setState({ inviteError: true, searchValue: '' });
    } else if (isValid) {
      this.addShip(searchTerm);
      this.setState({ searchValue: '' });
    }
  }

  render() {
    const { props, state } = this;
    let searchDisabled = props.disabled;
    if (props.invites.groups) {
      if (props.invites.groups.length > 0) {
        searchDisabled = true;
      }
    }

    let participants = <div />;
    let searchResults = <div />;

    let placeholder = '';
    if (props.shipResults) {
      placeholder = 'ships';
    }
    if (props.groupResults) {
      if (placeholder.length > 0) {
        placeholder = placeholder + ' or ';
      }
      placeholder = placeholder + 'existing groups';
    }
    placeholder = 'Search for ' + placeholder;

    let invErrElem = <span />;
    if (state.inviteError) {
      invErrElem = (
        <span className='f9 inter red2 db pt2'>
          Invited ships must be validly formatted ship names.
        </span>
      );
    }

    if (
      state.searchResults.groups.length > 0 ||
      state.searchResults.ships.length > 0
    ) {
      const groupHeader =
        state.searchResults.groups.length > 0 ? (
          <p className='f9 gray2 ph3 pb2'>Groups</p>
        ) : (
          ''
        );

      const shipHeader =
        state.searchResults.ships.length > 0 ? (
          <p className='f9 gray2 pv2 ph3'>Ships</p>
        ) : (
          ''
        );

      const groupResults = state.searchResults.groups.map((group) => {
        return (
          <li
            key={group[0]}
            className={
              'list white-d f8 pv2 ph3 pointer' +
              ' hover-bg-gray4 hover-bg-gray1-d ' +
              (group[1] ? 'inter' : 'mono') +
              (group[0] === state.selected ? ' bg-gray1-d bg-gray4' : '')
            }
            onClick={() => this.addGroup(group[0])}
          >
            <span className='mix-blend-diff white'>
              {group[1] ? group[1] : group[0]}
            </span>
          </li>
        );
      });

      const shipResults = Array.from(new Set(state.searchResults.ships)).map((ship) => {
        const nicknames = (this.state.contacts.get(ship) || [])
          .filter((e) => {
            return !(e === '');
          })
          .join(', ');

        return (
          <li
            key={ship}
            className={
              'list mono white-d f8 pv1 ph3 pointer' +
              ' hover-bg-gray4 hover-bg-gray1-d relative' +
              (ship === state.selected ? ' bg-gray1-d bg-gray4' : '')
            }
            onClick={(e) => this.addShip(ship)}
          >
            <Sigil
              ship={'~' + ship}
              size={24}
              color='#000000'
              classes='mix-blend-diff v-mid'
            />
            <span className='v-mid ml2 mw5 truncate dib mix-blend-diff white'>
              {'~' + ship}
            </span>
            <span className='absolute right-1 di truncate mw4 inter f9 pt1 mix-blend-diff white'>
              {nicknames}
            </span>
          </li>
        );
      });

      searchResults = (
        <div
          className={
            'absolute bg-white bg-gray0-d white-d' +
            ' pv3 z-1 w-100 mt1 ba b--white-d overflow-y-scroll mh-16'
          }
        >
          {groupHeader}
          {groupResults}
          {shipHeader}
          {shipResults}
        </div>
      );
    }

    const groupInvites = props.invites.groups || [];
    const shipInvites = props.invites.ships || [];

    if (groupInvites.length > 0 || shipInvites.length > 0) {
      const groups = groupInvites.map((group) => {
        return (
          <span
            key={group[0]}
            className={
              'f9 mono black pa2 bg-gray5 bg-gray1-d' +
              ' ba b--gray4 b--gray2-d white-d dib mr2 mt2 c-default'
            }
          >
            {group}
            <span
              className='white-d ml3 mono pointer'
              onClick={(e) => this.deleteGroup()}
            >
              x
            </span>
          </span>
        );
      });

      const ships = shipInvites.map((ship) => {
        return (
          <span
            key={ship}
            className={
              'f9 mono black pa2 bg-gray5 bg-gray1-d' +
              ' ba b--gray4 b--gray2-d white-d dib mr2 mt2 c-default'
            }
          >
            {'~' + ship}
            <span
              className='white-d ml3 mono pointer'
              onClick={(e) => this.deleteShip(ship)}
            >
              x
            </span>
          </span>
        );
      });

      participants = (
        <div
          className={
            'f9 gray2 bb bl br b--gray3 b--gray2-d bg-gray0-d ' +
            'white-d pa3 db w-100 inter'
          }
        >
          <span className='db gray2'>Participants</span>
          {groups} {ships}
        </div>
      );
    }

    return (
      <div className='relative'>
        <img
          src='/~landscape/img/search.png'
          className='absolute invert-d'
          style={{
            height: 16,
            width: 16,
            top: 14,
            left: 12,
          }}
        />
        <textarea
          ref={this.textarea}
          className={
            'f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 w-100' +
            ' db focus-b--black focus-b--white-d'
          }
          placeholder={placeholder}
          disabled={searchDisabled}
          rows={1}
          spellCheck={false}
          style={{
            resize: 'none',
            paddingLeft: 36,
          }}
          onChange={this.search}
          value={state.searchValue}
        />
        {searchResults}
        {participants}
        {invErrElem}
      </div>
    );
  }
}

export default InviteSearch;
