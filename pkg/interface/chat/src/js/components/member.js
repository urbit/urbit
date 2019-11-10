import React, { Component } from 'react';
import classnames from 'classnames';

import urbitOb from 'urbit-ob';
import { deSig } from '/lib/util';
import { ChatTabBar } from '/components/lib/chat-tabbar';
import { MemberElement } from '/components/lib/member-element';
import { InviteElement } from '/components/lib/invite-element';


export class MemberScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      station: `/${props.match.params.ship}/${props.match.params.station}`,
    };

  }

  render() {
    const { props, state } = this;

    let writeGroup = Array.from(props.write.who.values());
    let readGroup = Array.from(props.read.who.values());

    let writeText = '';
    let readText = '';
    let modWriteText = '';
    let modReadText = '';

    if (props.write.kind === 'black') {
      writeText = 'Everyone banned from writing to this chat.';
      modWriteText = 'Ban someone from writing to this chat.';
    } else if (props.write.kind === 'white') {
      writeText = 'Everyone with permission to message this chat.';
      modWriteText = 'Invite someone to write to this chat.';
    }

    if (props.read.kind === 'black') {
      readText = 'Everyone banned from reading this chat.';
      modReadText = 'Ban someone from reading this chat.';
    } else if (props.read.kind === 'white') {
      readText = 'Everyone with permission to read this chat.';
      modReadText = 'Invite someone to read this chat.';
    }

    let writeListMembers = writeGroup.map((mem) => {
      return (
        <MemberElement 
          key={mem} 
          owner={deSig(props.match.params.ship)}
          ship={mem}
          path={`/chat${state.station}/write`}
          kind={props.write.kind}
          api={props.api}  />
      );
    });

    let readListMembers = readGroup.map((mem) => {
      return (
        <MemberElement 
          key={mem} 
          owner={deSig(props.match.params.ship)}
          ship={mem}
          path={`/chat${state.station}/read`}
          kind={props.read.kind}
          api={props.api} />
      );
    });

    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column">
        <div className='pl3 pt2 bb mb3'>
          <h2>{state.station.substr(1)}</h2>
          <ChatTabBar
            {...props}
            station={state.station}
            numPeers={writeGroup.length}
            isOwner={deSig(props.match.params.ship) === window.ship} />
        </div>
        <div className="w-100 cf">
          <div className="w-50 fl pa2 pr3">
            <p className="body-regular mb3">Members</p>
            <p className="label-regular gray mb3">{writeText}</p>
            {writeListMembers}
          </div>
          <div className="w-50 fr pa2 pl3">
            <p className="body-regular mb3">Modify Permissions</p>
            <p className="label-regular gray mb3">
              {modWriteText}
            </p>
            { window.ship === deSig(props.match.params.ship) ? (
              <InviteElement
                path={`/chat${state.station}/write`}
                station={`/${props.match.params.station}`}
                permissions={props.write}
                api={props.api} />
            ) : null }
          </div>
        </div>
        <div className="w-100 cf mt2">
          <div className="w-50 fl pa2 pr3">
            <p className="label-regular gray mb3">{readText}</p>
            {readListMembers}
          </div>
          <div className="w-50 fr pa2 pl3">
            <p className="label-regular gray mb3">
              {modReadText}
            </p>
            { window.ship === deSig(props.match.params.ship) ?
              (  <InviteElement
                   path={`/chat${state.station}/read`}
                   station={`/${props.match.params.station}`}
                   permissions={props.read}
                   api={props.api}/>
              ) : null
            }
          </div>
        </div>
      </div>
    )
  }
}
