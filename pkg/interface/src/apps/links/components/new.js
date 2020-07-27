import React, { Component } from 'react';
import { InviteSearch } from '../../../components/InviteSearch';
import { Spinner } from '../../../components/Spinner';
import { Link } from 'react-router-dom';
import { makeRoutePath, deSig } from '../../../lib/util';
import urbitOb from 'urbit-ob';

export class NewScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      title: '',
      description: '',
      idName: '',
      groups: [],
      ships: [],
      idError: false,
      inviteError: false,
      createGroup: false,
      disabled: false
    };

    this.titleChange = this.titleChange.bind(this);
    this.descriptionChange = this.descriptionChange.bind(this);
    this.setInvite = this.setInvite.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;

    if (prevProps !== props) {
      const target = `/${state.idName}`;
      if (target in props.associations) {
        props.history.push(makeRoutePath(target));
      }
    }
  }

  titleChange(event) {
    const asciiSafe = event.target.value.toLowerCase()
      .replace(/[^a-z0-9-]/g, '-');
    this.setState({
      idName: asciiSafe + '-' + Math.floor(Math.random()*10000), // uniqueness
      title: event.target.value
    });
  }

  descriptionChange(event) {
    this.setState({
      description: event.target.value
    });
  }

  setInvite(value) {
    this.setState({
      groups: value.groups,
      ships: value.ships
    });
  }

  onClickCreate() {
    const { props, state } = this;

    if (!state.title) {
      this.setState({
        idError: true,
        inviteError: false
      });
      return;
    }

    const appPath = `/${state.idName}`;

    if (appPath in props.associations) {
      this.setState({
        inviteError: false,
        idError: true,
        success: false
      });
      return;
    }

    let isValid = true;
    const aud = state.ships.map(mem => `~${deSig(mem.trim())}`);
    aud.forEach((mem) => {
      if (!urbitOb.isValidPatp(mem)) {
        isValid = false;
      }
    });

    if (!isValid) {
      this.setState({
        inviteError: true,
        idError: false,
        success: false
      });
      return;
    }

    const target = state.groups.length === 0
      ? { ships: aud }
      : { group: state.groups[0] };

    if (this.textarea) {
      this.textarea.value = '';
    }

    this.setState({
      error: false,
      success: true,
      group: [],
      ships: [],
      disabled: true
    }, () => {
      const submit = props.api.links.createCollection(
        appPath,
        state.title,
        state.description,
        target,
        state.createGroup
      );
      submit.then(() => {
        this.setState({ disabled: false });
        props.history.push(makeRoutePath(appPath));
      });
    });
  }

  render() {
    const { props, state } = this;

    const createClasses = state.idName
      ? 'pointer db f9 mt7 green2 bg-gray0-d ba pv3 ph4 b--green2'
      : 'pointer db f9 mt7 gray2 ba bg-gray0-d pa2 pv3 ph4 b--gray3';

    const idClasses =
      'f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 ' +
      'focus-b--black focus-b--white-d ';

    let idErrElem = (<span />);
    if (state.idError) {
      idErrElem = (
        <span className="f9 inter red2 db pt2">
          Collection must have a valid name.
        </span>
      );
    }

    return (
      <div
        className={
          'h-100 w-100 mw6 pa3 pt4 overflow-x-hidden ' +
          'bg-gray0-d white-d flex flex-column'
        }
      >
        <div className="w-100 dn-m dn-l dn-xl inter pt1 pb6 f8">
          <Link to="/~link">{'⟵ All Collections'}</Link>
        </div>
        <h2 className="mb3 f8">New Collection</h2>
        <div className="w-100">
          <p className="f8 mt3 lh-copy db">Name</p>
          <textarea
            className={idClasses}
            placeholder="Cool Collection"
            rows={1}
            style={{
              resize: 'none'
            }}
            onChange={this.titleChange}
          />
              {idErrElem}
          <p className="f8 mt3 lh-copy db">
            Description
            <span className="gray3"> (Optional)</span>
          </p>
          <textarea
            className={idClasses}
            placeholder="The hippest links"
            rows={1}
            style={{
              resize: 'none'
            }}
            onChange={this.descriptionChange}
          />
          <div className="mt4 db relative">
            <p className="f8">
              Invite
              <span className="gray3"> (Optional)</span>
          </p>
            <Link className="green2 absolute right-0 bottom-0 f9" to="/~groups/new">Create Group</Link>
            <p className="f9 gray2 db mv1">
              Selected group or ships will be invited to the collection
          </p>
          </div>
          <InviteSearch
            associations={props.associations.contacts}
            groups={props.groups}
            contacts={props.contacts}
            groupResults={true}
            shipResults={true}
            invites={{
              groups: state.groups,
              ships: state.ships
            }}
            setInvite={this.setInvite}
          />
          <button
            onClick={this.onClickCreate.bind(this)}
            className={createClasses}
            disabled={this.state.disabled}
          >
            Create Collection
          </button>
          <Spinner awaiting={this.state.disabled} classes="mt3" text="Creating collection..." />
        </div>
      </div>
    );
  }
}
