import React, { Component } from 'react';
import Toggle from '~/views/components/toggle';
import { InviteSearch } from '~/views/components/InviteSearch';


export class GroupifyButton extends Component {

  constructor(props) {
    super(props);

    this.state = {
      inclusive: false,
      targetGroup: null
    };
  }

  changeTargetGroup(target) {
    if (target.groups.length === 1) {
      this.setState({ targetGroup: target.groups[0] });
    } else {
      this.setState({ targetGroup: null });
    }
  }

  changeInclusive(event) {
    this.setState({ inclusive: Boolean(event.target.checked) });
  }

  renderInclusiveToggle() {
    return this.state.targetGroup ? (
      <div className="mt4">
      <Toggle
        boolean={inclusive}
        change={this.changeInclusive.bind(this)}
      />
        <span className="dib f9 white-d inter ml3">
          Add all members to group
        </span>
        <p className="f9 gray2 pt1" style={{ paddingLeft: 40 }}>
          Add chat members to the group if they aren't in it yet
        </p>
      </div>
    ) : <div />;
  }
  
  render() {
    const { inclusive, targetGroup } = this.state;
    const {
      api,
      isOwner,
      association,
      associations,
      contacts,
      groups,
      station
    } = this.props;

    const groupPath = association['group-path'];
    const ownedUnmanagedVillage =
      isOwner &&
      !contacts[groupPath];

    if (!ownedUnmanagedVillage) {
      return null;
    }

    return (
      <div className={'w-100 fl mt3'} style={{ maxWidth: '29rem' }}>
        <p className="f8 mt3 lh-copy db">Convert Chat</p>
        <p className="f9 gray2 db mb4">
          Convert this chat into a group with associated chat, or select a
          group to add this chat to.
        </p>
        <InviteSearch
          groups={groups}
          contacts={contacts}
          associations={associations}
          groupResults={true}
          shipResults={false}
          invites={{
            groups: targetGroup ? [targetGroup] : [],
            ships: []
          }}
          setInvite={this.changeTargetGroup.bind(this)}
        />
        {this.renderInclusiveToggle()}
        <a onClick={() => {
          changeLoading(true, true, 'Converting to group...', () => {
            api.chat.groupify(
              station, targetGroup, inclusive
            ).then(() => {
              changeLoading(false, false, '', () => {});
            });
          });
        }}
           className={
             'dib f9 black gray4-d bg-gray0-d ba pa2 mt4 b--black ' + 
             'b--gray1-d pointer'
           }>Convert to group</a>
      </div>
    );
  }
}

