import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { Spinner } from '../../../components/Spinner';
import urbitOb from 'urbit-ob';

export class JoinScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      group: '',
      error: false,
      awaiting: null,
      disable: false
    };

    this.groupChange = this.groupChange.bind(this);
  }

  componentDidMount() {
    this.componentDidUpdate();
  }

  componentDidUpdate(prevProps) {
    const { props, state } = this;
    // autojoin by URL, waits for group information
    if ((props.ship && props.name) &&
    (prevProps && (prevProps.groups !== props.groups))) {
      console.log('autojoining');
      const incomingGroup = `${props.ship}/${props.name}`;
      // push to group if already exists
      if (`/ship/${incomingGroup}` in props.groups) {
        this.props.history.push(`/~groups/ship/${incomingGroup}`);
        return;
      }
      this.setState({ group: incomingGroup }, () => {
        this.onClickJoin();
      });
    }
    // once we've joined, push to group page
    if (props.groups) {
      if (state.awaiting) {
        const group = `/ship/${state.group}`;
        if (group in props.groups) {
          props.history.push(`/~groups${group}`);
        }
      }
    }
  }


  onClickJoin() {
    const { props, state } = this;
    console.log('i am joining');

    const { group } = state;
    const [ship, name] = group.split('/');

    const text = 'Joining group';

    this.props.api.contacts.join({ ship, name }).then(() => {
      this.setState({ awaiting: text });
    });
  }

  groupChange(event) {
    const [ship, name] = event.target.value.split('/');
    const validGroup = urbitOb.isValidPatp(ship);
    this.setState({
      group: event.target.value,
      error: !validGroup
    });
  }

  render() {
    const { state } = this;

    let joinClasses = 'db f9 green2 ba pa2 b--green2 bg-gray0-d pointer';

    let errElem = (<span />);
    if (state.error) {
      joinClasses = 'db f9 gray2 ba pa2 b--gray3 bg-gray0-d';
      errElem = (
        <span className="f9 inter red2 db">
          Group must have a valid name.
        </span>
      );
    }

    return (
      <div className={'h-100 w-100 pt4 overflow-x-hidden flex flex-column ' +
      'bg-gray0-d white-d pa3'}
      >
        <div
          className="w-100 dn-m dn-l dn-xl inter pt1 pb6 f8"
        >
          <Link to="/~groups/">{'⟵ All Groups'}</Link>
        </div>
        <h2 className="mb3 f8">Join an Existing Group</h2>
        <div className="w-100">
          <p className="f8 lh-copy mt3 db">Enter a <span className="mono">~ship/group-name</span></p>
          <p className="f9 gray2 mb4">Group names use lowercase, hyphens, and slashes.</p>
          <textarea
            ref={ (e) => {
            this.textarea = e;
            } }
            className={'f7 mono ba bg-gray0-d white-d pa3 mb2 db ' +
            'focus-b--black focus-b--white-d b--gray3 b--gray2-d nowrap '}
            placeholder="~zod/group-name"
            spellCheck="false"
            rows={1}
            onKeyPress={(e) => {
              if (e.key === 'Enter') {
                e.preventDefault();
                this.onClickJoin();
              }
            }}
            style={{
              resize: 'none'
            }}
            onChange={this.groupChange}
            value={this.state.group}
          />
          {errElem}
          <br />
          <button
            disabled={this.state.error}
            onClick={this.onClickJoin.bind(this)}
            className={joinClasses}
          >Join Group</button>
          <Spinner awaiting={this.state.awaiting} classes="mt4" text="Joining group..." />
        </div>
      </div>
    );
  }
}
