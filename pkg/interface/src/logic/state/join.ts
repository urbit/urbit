import { useOsDark } from './local';
import { useTheme } from './settings';

export function useDark() {
  const osDark = useOsDark();
  const theme = useTheme();
  return theme === 'dark' || (osDark && theme === 'auto');
}
