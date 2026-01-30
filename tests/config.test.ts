/**
 * DeepClause CLI Config Module Tests
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { vol } from 'memfs';
import {
  initConfig,
  loadConfig,
  validateConfig,
  setModel,
  showModel,
  updateConfig,
  configExists,
  getConfigDir,
  getConfigPath,
  getToolsDir,
  getMCPServers,
  parseModelString,
  formatModelString,
  ConfigSchema,
  type Config,
  type Provider
} from '../src/cli/config.js';

// Mock fs module
vi.mock('fs/promises', async () => {
  const memfs = await import('memfs');
  return memfs.fs.promises;
});

describe('Config Module', () => {
  beforeEach(() => {
    vol.reset();
  });

  afterEach(() => {
    vol.reset();
  });

  // ===========================================================================
  // Path Helpers
  // ===========================================================================

  describe('Path Helpers', () => {
    it('should return correct config directory path', () => {
      expect(getConfigDir('/workspace')).toBe('/workspace/.deepclause');
    });

    it('should return correct config file path', () => {
      expect(getConfigPath('/workspace')).toBe('/workspace/.deepclause/config.json');
    });

    it('should return correct tools directory path', () => {
      expect(getToolsDir('/workspace')).toBe('/workspace/.deepclause/tools');
    });
  });

  // ===========================================================================
  // Config Validation
  // ===========================================================================

  describe('validateConfig', () => {
    it('should accept valid minimal config', () => {
      const config = { model: 'gpt-4o' };
      const result = validateConfig(config);
      
      expect(result.model).toBe('gpt-4o');
      expect(result.providers).toEqual({});
      expect(result.mcp).toEqual({ servers: {} });
    });

    it('should accept valid full config', () => {
      const config = {
        model: 'claude-3-opus',
        providers: {
          anthropic: { apiKey: 'test-key' }
        },
        mcp: {
          servers: {
            'brave-search': {
              command: 'npx',
              args: ['-y', '@anthropic/mcp-brave-search'],
              env: { BRAVE_API_KEY: 'key' }
            }
          }
        },
        dmlBase: 'custom/tools',
        workspace: './data'
      };
      
      const result = validateConfig(config);
      expect(result.model).toBe('claude-3-opus');
      expect(result.providers?.anthropic?.apiKey).toBe('test-key');
      expect(result.mcp?.servers?.['brave-search']?.command).toBe('npx');
    });

    it('should reject config without model', () => {
      const config = {};
      expect(() => validateConfig(config)).toThrow(/model/i);
    });

    it('should reject empty model', () => {
      const config = { model: '' };
      expect(() => validateConfig(config)).toThrow(/model/i);
    });

    it('should reject MCP server without command', () => {
      const config = {
        model: 'gpt-4o',
        mcp: {
          servers: {
            'test-server': {}
          }
        }
      };
      expect(() => validateConfig(config)).toThrow(/command/i);
    });

    it('should use default values for optional fields', () => {
      const config = { model: 'gpt-4o' };
      const result = validateConfig(config);
      
      expect(result.dmlBase).toBe('.deepclause/tools');
      expect(result.workspace).toBe('./');
    });
  });

  // ===========================================================================
  // initConfig
  // ===========================================================================

  describe('initConfig', () => {
    it('should create config directory structure', async () => {
      vol.fromJSON({});
      
      await initConfig('/workspace');
      
      expect(vol.existsSync('/workspace/.deepclause')).toBe(true);
      expect(vol.existsSync('/workspace/.deepclause/config.json')).toBe(true);
      expect(vol.existsSync('/workspace/.deepclause/tools')).toBe(true);
      expect(vol.existsSync('/workspace/.deepclause/.gitignore')).toBe(true);
    });

    it('should create valid default config', async () => {
      vol.fromJSON({});
      
      await initConfig('/workspace');
      
      const content = vol.readFileSync('/workspace/.deepclause/config.json', 'utf-8') as string;
      const config = JSON.parse(content);
      
      expect(config.model).toBe('gpt-4o');
      expect(validateConfig(config)).toBeTruthy();
    });

    it('should use custom model when specified', async () => {
      vol.fromJSON({});
      
      await initConfig('/workspace', { model: 'claude-3-opus' });
      
      const content = vol.readFileSync('/workspace/.deepclause/config.json', 'utf-8') as string;
      const config = JSON.parse(content);
      
      expect(config.model).toBe('claude-3-opus');
    });

    it('should fail if config exists without --force', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({ model: 'existing' })
      });
      
      await expect(initConfig('/workspace')).rejects.toThrow(/already exists/);
    });

    it('should overwrite config with --force', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({ model: 'existing' })
      });
      
      await initConfig('/workspace', { force: true });
      
      const content = vol.readFileSync('/workspace/.deepclause/config.json', 'utf-8') as string;
      const config = JSON.parse(content);
      
      expect(config.model).toBe('gpt-4o');
    });
  });

  // ===========================================================================
  // loadConfig
  // ===========================================================================

  describe('loadConfig', () => {
    it('should load valid config', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({ model: 'gpt-4o' })
      });
      
      const config = await loadConfig('/workspace');
      
      expect(config.model).toBe('gpt-4o');
    });

    it('should fail if config not found', async () => {
      vol.fromJSON({});
      
      await expect(loadConfig('/workspace')).rejects.toThrow(/not found/);
    });

    it('should fail on invalid JSON', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': 'not valid json {'
      });
      
      await expect(loadConfig('/workspace')).rejects.toThrow();
    });

    it('should resolve environment variables', async () => {
      process.env.TEST_API_KEY = 'resolved-key';
      
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({
          model: 'gpt-4o',
          providers: {
            openai: { apiKey: '${TEST_API_KEY}' }
          }
        })
      });
      
      const config = await loadConfig('/workspace');
      
      expect(config.providers?.openai?.apiKey).toBe('resolved-key');
      
      delete process.env.TEST_API_KEY;
    });

    it('should preserve unresolved env vars', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({
          model: 'gpt-4o',
          providers: {
            openai: { apiKey: '${NONEXISTENT_VAR}' }
          }
        })
      });
      
      const config = await loadConfig('/workspace');
      
      expect(config.providers?.openai?.apiKey).toBe('${NONEXISTENT_VAR}');
    });
  });

  // ===========================================================================
  // parseModelString / formatModelString
  // ===========================================================================

  describe('parseModelString', () => {
    it('should parse provider/model format', () => {
      const result = parseModelString('anthropic/claude-3-opus');
      expect(result.provider).toBe('anthropic');
      expect(result.model).toBe('claude-3-opus');
    });

    it('should parse openai provider', () => {
      const result = parseModelString('openai/gpt-4o');
      expect(result.provider).toBe('openai');
      expect(result.model).toBe('gpt-4o');
    });

    it('should parse google provider', () => {
      const result = parseModelString('google/gemini-2.0-flash');
      expect(result.provider).toBe('google');
      expect(result.model).toBe('gemini-2.0-flash');
    });

    it('should parse openrouter provider', () => {
      const result = parseModelString('openrouter/meta-llama/llama-3-70b');
      expect(result.provider).toBe('openrouter');
      expect(result.model).toBe('meta-llama/llama-3-70b');
    });

    it('should infer openai from gpt- prefix', () => {
      const result = parseModelString('gpt-4-turbo');
      expect(result.provider).toBe('openai');
      expect(result.model).toBe('gpt-4-turbo');
    });

    it('should infer openai from o1/o3 prefix', () => {
      expect(parseModelString('o1-preview').provider).toBe('openai');
      expect(parseModelString('o3-mini').provider).toBe('openai');
    });

    it('should infer anthropic from claude- prefix', () => {
      const result = parseModelString('claude-3-sonnet');
      expect(result.provider).toBe('anthropic');
      expect(result.model).toBe('claude-3-sonnet');
    });

    it('should infer google from gemini- prefix', () => {
      const result = parseModelString('gemini-pro');
      expect(result.provider).toBe('google');
      expect(result.model).toBe('gemini-pro');
    });

    it('should default to openrouter for unknown models', () => {
      const result = parseModelString('some-other-model');
      expect(result.provider).toBe('openrouter');
      expect(result.model).toBe('some-other-model');
    });

    it('should reject empty string', () => {
      expect(() => parseModelString('')).toThrow(/invalid model/i);
    });

    it('should reject whitespace-only', () => {
      expect(() => parseModelString('   ')).toThrow(/invalid model/i);
    });

    it('should reject unknown provider', () => {
      expect(() => parseModelString('unknown/model')).toThrow(/unknown provider/i);
    });
  });

  describe('formatModelString', () => {
    it('should format provider/model', () => {
      expect(formatModelString('openai', 'gpt-4o')).toBe('openai/gpt-4o');
      expect(formatModelString('anthropic', 'claude-3-opus')).toBe('anthropic/claude-3-opus');
    });
  });

  // ===========================================================================
  // setModel / showModel
  // ===========================================================================

  describe('setModel', () => {
    it('should update model and provider in config', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({ model: 'gpt-4o', provider: 'openai' })
      });
      
      const result = await setModel('/workspace', 'anthropic/claude-3-opus');
      
      expect(result.provider).toBe('anthropic');
      expect(result.model).toBe('claude-3-opus');
      
      const content = vol.readFileSync('/workspace/.deepclause/config.json', 'utf-8') as string;
      const config = JSON.parse(content);
      
      expect(config.model).toBe('claude-3-opus');
      expect(config.provider).toBe('anthropic');
    });

    it('should infer provider when not specified', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({ model: 'gpt-4o', provider: 'openai' })
      });
      
      const result = await setModel('/workspace', 'gemini-2.0-flash');
      
      expect(result.provider).toBe('google');
      expect(result.model).toBe('gemini-2.0-flash');
    });

    it('should reject empty model', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({ model: 'gpt-4o', provider: 'openai' })
      });
      
      await expect(setModel('/workspace', '')).rejects.toThrow(/invalid model/i);
    });

    it('should reject whitespace-only model', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({ model: 'gpt-4o', provider: 'openai' })
      });
      
      await expect(setModel('/workspace', '   ')).rejects.toThrow(/invalid model/i);
    });
  });

  describe('showModel', () => {
    it('should return provider and model', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({ model: 'claude-3-opus', provider: 'anthropic' })
      });
      
      const result = await showModel('/workspace');
      
      expect(result.provider).toBe('anthropic');
      expect(result.model).toBe('claude-3-opus');
      expect(result.formatted).toBe('anthropic/claude-3-opus');
    });
  });

  // ===========================================================================
  // updateConfig
  // ===========================================================================

  describe('updateConfig', () => {
    it('should merge updates into config', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({
          model: 'gpt-4o',
          workspace: './old'
        })
      });
      
      const updated = await updateConfig('/workspace', { workspace: './new' });
      
      expect(updated.model).toBe('gpt-4o');
      expect(updated.workspace).toBe('./new');
    });

    it('should validate after update', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({ model: 'gpt-4o' })
      });
      
      await expect(updateConfig('/workspace', { model: '' })).rejects.toThrow();
    });
  });

  // ===========================================================================
  // configExists
  // ===========================================================================

  describe('configExists', () => {
    it('should return true when config exists', async () => {
      vol.fromJSON({
        '/workspace/.deepclause/config.json': JSON.stringify({ model: 'gpt-4o' })
      });
      
      expect(await configExists('/workspace')).toBe(true);
    });

    it('should return false when config missing', async () => {
      vol.fromJSON({});
      
      expect(await configExists('/workspace')).toBe(false);
    });
  });

  // ===========================================================================
  // getMCPServers
  // ===========================================================================

  describe('getMCPServers', () => {
    it('should return MCP servers from config', () => {
      const config: Config = {
        model: 'gpt-4o',
        providers: {},
        mcp: {
          servers: {
            'test-server': { command: 'test', args: [], env: {} }
          }
        },
        dmlBase: '.deepclause/tools',
        workspace: './'
      };
      
      const servers = getMCPServers(config);
      
      expect(servers).toHaveProperty('test-server');
      expect(servers['test-server'].command).toBe('test');
    });

    it('should return empty object when no servers', () => {
      const config: Config = {
        model: 'gpt-4o',
        providers: {},
        mcp: { servers: {} },
        dmlBase: '.deepclause/tools',
        workspace: './'
      };
      
      const servers = getMCPServers(config);
      
      expect(servers).toEqual({});
    });
  });
});

// ===========================================================================
// Schema Tests
// ===========================================================================

describe('ConfigSchema', () => {
  it('should be a valid Zod schema', () => {
    expect(ConfigSchema).toBeDefined();
    expect(typeof ConfigSchema.parse).toBe('function');
  });

  it('should parse minimal config', () => {
    const result = ConfigSchema.parse({ model: 'test' });
    expect(result.model).toBe('test');
  });

  it('should fail on invalid input', () => {
    expect(() => ConfigSchema.parse({})).toThrow();
  });
});
