import React, { Component } from 'react';
import { getStationDetails } from '/services';

export class ChatList extends Component {
  componentDidMount() {
    let path = `/public`;

    this.props.api.bind(path, "PUT", this.props.hostship.slice(1));
  }

  renderChats() {
    if (this.props.store.public[this.props.hostship]) {
      const chats = this.props.store.public[this.props.hostship].map((cir) => {
        const deets = getStationDetails(cir)
        if (deets.type == "stream-chat" || deets.type == "stream-dm") {
          return (
            <div className="mt-2 text-500">
              <a href={`/~chat/{cir}`}>{cir}</a>
            </div>
          )
        } else {
          return null;
        }
      });
      return chats;
    } else {
      return null;
    }
  }

  render() {
    const chats = this.renderChats();
    return (
      <div>
        <div className="text-600 mt-8">Chats</div>
        {chats}
      </div>
    );
  }
}
