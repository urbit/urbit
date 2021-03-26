import { Box, BoxProps, Icon, Text } from '@tlon/indigo-react';
import React, { ReactElement } from 'react';
import { Link } from 'react-router-dom';

type BackButtonProps = BoxProps & {
  text?: string;
  to: string;
}
const BackButton = ({ text = 'Back', to, ...rest }: BackButtonProps): ReactElement => {
  return (
    <Link to={to}>
      <Box display="flex" alignItems="center" {...rest}>
        <Icon icon="ChevronWest" bg="washedGray" borderRadius="50%" p={1} size="12px" mr={1}></Icon>
        <Text bold>{text}</Text>
      </Box>
    </Link>
  );
};

export default BackButton;
