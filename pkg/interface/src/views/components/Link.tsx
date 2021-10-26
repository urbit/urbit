import React from 'react';
import { Link as RouterLink } from 'react-router-dom';
import { ActionProps, asAction } from '@tlon/indigo-react';
import { PropFunc } from '~/types';

interface AsLinkProps {
  to: PropFunc<typeof RouterLink>['to'];
  replace?: boolean;
}

export type ActionLinkProps = AsLinkProps & ActionProps;

export const ActionLink: React.FC<ActionLinkProps> = asAction(RouterLink);
