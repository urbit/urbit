import React from 'react';
import { Row, Text } from '@tlon/indigo-react';
import styled from 'styled-components';

interface AutocompletePatpProps {
  suggestions: string[];
  isAdmin: boolean;
  mentionCursor: number;
  enteredUser: string;
  selectMember: (member: string) => () => void;
  inviteMissingUser: () => Promise<void>;
}

const AutocompleteSuggestionRow = styled(Row)`
  color: rgba(33,157,255,1);
  height: 28px;
  cursor: pointer;
  padding: 6px 45px;
  &:hover {
    text-decoration: underline;
  }
`;

export function AutocompletePatp({
  suggestions,
  isAdmin,
  mentionCursor,
  enteredUser,
  selectMember,
  inviteMissingUser
}: AutocompletePatpProps) {
  if (suggestions.length) {
    return <>
      {suggestions.map((suggestion, i) => (
        <AutocompleteSuggestionRow
          key={suggestion}
          onClick={selectMember(suggestion)}
          backgroundColor={mentionCursor === i ? 'washedGray' : undefined}
          ref={(ele) => {
            if (ele && mentionCursor === i) {
              ele.scrollIntoView();
            }
          }}
        >
          <Text mono color="rgba(33,157,255,1)">{suggestion}</Text>
        </AutocompleteSuggestionRow>
      ))}
    </>;
  } else if (isAdmin) {
    return <AutocompleteSuggestionRow onClick={inviteMissingUser} whiteSpace="nowrap">
      <Text>
        Invite <Text mono color="rgba(33,157,255,1)">{enteredUser}</Text> to group
      </Text>
    </AutocompleteSuggestionRow>;
  }

  // TODO?: if group is public and mention is not in group, give option to share group with user
  return <AutocompleteSuggestionRow>
    <Text whiteSpace="nowrap">
      <Text mono color="rgba(33,157,255,1)">{enteredUser}</Text> is not in this group
    </Text>
  </AutocompleteSuggestionRow>;
}
