import { Text } from '@tlon/indigo-react';
import styled from 'styled-components';

export const TruncatedText = styled(Text)`
  white-space: pre;
  text-overflow: ellipsis;
  overflow: hidden;
  min-width: 0;
`;

