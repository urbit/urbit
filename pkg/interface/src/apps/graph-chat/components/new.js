import React, { Component } from 'react';
import { Spinner } from '../../../components/Spinner';
import { Link } from 'react-router-dom';
import { deSig } from '../../../lib/util';
import urbitOb from 'urbit-ob';


export class NewScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      title: '',
      awaiting: false
    };

    this.titleChange = this.titleChange.bind(this);
  }

  componentDidUpdate(prevProps, prevState) {
    const { props, state } = this;

    if (prevProps !== props) {
      const resource = `~${window.ship}/${state.idName}`;
      if (resource in props.graphs) {
        props.history.push('/~chat/room/' + resource);
      }
    }
  }

  titleChange(event) {
    const asciiSafe = event.target.value.toLowerCase()
      .replace(/[^a-z0-9~_.-]/g, '-');
    this.setState({
      idName: asciiSafe,
      title: event.target.value
    });
  }

  onClickCreate() {
    const { props, state } = this;

    this.setState({
      awaiting: true
    }, () => {
      props.api.addGraph(`~${window.ship}`, state.title, {});
    });
  }

  render() {
    const { props, state } = this;
    const createClasses = state.idName
      ? 'pointer db f9 green2 bg-gray0-d ba pv3 ph4 b--green2'
      : 'pointer db f9 gray2 ba bg-gray0-d pa2 pv3 ph4 b--gray3';

    const idClasses =
      'f7 ba b--gray3 b--gray2-d bg-gray0-d white-d pa3 db w-100 ' +
      'focus-b--black focus-b--white-d ';

    return (
      <div
        className={
          'h-100 w-100 mw6 pa3 pt4 overflow-x-hidden ' +
          'bg-gray0-d white-d flex flex-column'
        }
      >
        <div className="w-100 dn-m dn-l dn-xl inter pt1 pb6 f8">
          <Link to="/~chat/">{'⟵ All Chats'}</Link>
        </div>
        <h2 className="mb3 f8">New Chat</h2>
        <div className="w-100">
          <p className="f8 mt3 lh-copy db">Name</p>
          <textarea
            className={idClasses}
            placeholder="Secret Chat"
            rows={1}
            style={{
              resize: 'none'
            }}
            onChange={this.titleChange}
          />
          <button
            onClick={this.onClickCreate.bind(this)}
            className={createClasses}
          >
            Start Chat
          </button>
          <Spinner awaiting={this.state.awaiting} classes="mt4" text="Creating chat..." />
        </div>
      </div>
    );
  }
}
