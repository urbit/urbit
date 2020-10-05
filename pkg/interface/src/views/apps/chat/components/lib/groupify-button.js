import React, { Component } from 'react';
import Toggle from '~/views/components/toggle';
import { InviteSearch } from '~/views/components/InviteSearch';

import { Button, Text, Box } from '@tlon/indigo-react';

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

  renderInclusiveToggle(inclusive) {
    return this.state.targetGroup ? (
      <Box mt='4'>
      <Toggle
        boolean={this.state.inclusive}
        change={this.changeInclusive.bind(this)}
      />
        <Text display='inline-block' fontSize='0' ml='3'>
          Add all members to group
        </Text>
        <Text display='block' fontSize='0' gray pt='1' pl='40px'>
          Add chat members to the group if they aren't in it yet
        </Text>
      </Box>
    ) : <Box />;
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
      station,
      changeLoading
    } = this.props;

    const groupPath = association['group-path'];
    const ownedUnmanagedVillage =
      isOwner &&
      !contacts[groupPath];

    if (!ownedUnmanagedVillage) {
      return null;
    }

    return (
      <Box width='100%' mt='3' maxWidth='29rem'>
        <Text display='block' fontSize='1' mt='3' mb='1'>Convert Chat</Text>
        <Text gray display='block' mb='4' fontSize='0'>
          Convert this chat into a group with associated chat, or select a
          group to add this chat to
        </Text>
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
        {this.renderInclusiveToggle(inclusive)}
        <Button mt='3' onClick={() => {
          changeLoading(true, true, 'Converting to group...', () => {
            api.chat.groupify(
              station, targetGroup, inclusive
            ).then(() => {
              changeLoading(false, false, '', () => {});
            });
          });
        }}
        >Convert to group</Button>
      </Box>
    );
  }
}

