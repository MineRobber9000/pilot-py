class Registers:
    def __init__(self):
        self.ab = 0
        self.hl = 0
        self.ix = 0
        self.ds = 0
        self.c = 0
    @property
    def a(self):
        return (self.ab>>8)&0xFF
    @a.setter
    def a(self,v):
        self.ab = (self.ab&0xFF)|((v&0xFF)<<8)
    @property
    def asx(self):
        # sign-extend a to 16 bits
        sign_bit = 1<<7
        a = self.a # cache
        return ((a&(sign_bit-1))-(a&sign_bit))&0xFFFF
    @property
    def azx(self):
        # zero-extend a to 16 bits
        return self.a
    @property
    def b(self):
        return self.ab&0xFF
    @b.setter
    def b(self,v):
        self.ab = (self.ab&0xFF00)|(v&0xFF)
    @property
    def bsx(self):
        # sign-extend b to 16 bits
        sign_bit = 1<<7
        b = self.b # cache
        return ((b&(sign_bit-1))-(b&sign_bit))&0xFFFF
    @property
    def h(self):
        return (self.hl>>8)&0xFF
    @h.setter
    def h(self,v):
        self.hl = (self.hl&0xFF)|((v&0xFF)<<8)
    @property
    def l(self):
        return self.hl&0xFF
    @l.setter
    def l(self,v):
        self.hl = (self.hl&0xFF00)|(v&0xFF)
    @property
    def abhl(self):
        return ((self.ab&0xFFFF)<<16)|(self.hl&0xFFFF)
    @abhl.setter
    def abhl(self,v):
        self.hl = v&0xFFFF
        self.ab = (v>>16)&0xFFFF
    @property
    def i(self):
        return (self.ix>>8)&0xFF
    @i.setter
    def i(self,v):
        self.ix = (self.ix&0xFF)|((v&0xFF)<<8)
    @property
    def x(self):
        return self.ix&0xFF
    @x.setter
    def x(self,v):
        self.ix = (self.ix&0xFF00)|(v&0xFF)
    @property
    def d(self):
        return (self.ds>>8)&0xFF
    @d.setter
    def d(self,v):
        self.ds = (self.ds&0xFF)|((v&0xFF)<<8)
    @property
    def s(self):
        return self.ds&0xFF
    @s.setter
    def s(self,v):
        self.ds = (self.ds&0xFF00)|(v&0xFF)

class SystemRegisters:
    def __init__(self):
        self._sp = 0
        self._pc = 0
        self.kf = 0
        self.shadow_f = 0
        self.ie = 0
    @property
    def k(self):
        return (self.kf>>8)&0xFF
    @k.setter
    def k(self,v):
        self.kf = (self.kf&0xFF)|((v&0xFF)<<8)
    @property
    def f(self):
        return self.kf&0xFF
    @f.setter
    def f(self,v):
        self.kf = (self.kf&0xFF00)|(v&0xFF)
    @property
    def pc(self):
        return (self.k<<16) | (self._pc<<1)
    @pc.setter
    def pc(self,v):
        self._pc = (v&0xFFFF)>>1
        self.k = (v>>16)&0xFF
    @property
    def pc_only(self):
        return self._pc<<1
    @pc_only.setter
    def pc_only(self,v):
        self._pc = (v&0xFFFF)>>1
    @property
    def sp(self):
        return self._sp<<1
    @sp.setter
    def sp(self,v):
        self._sp = (v&0xFFFF)>>1
    def __get_flag(self,b):
        return (self.f&(1<<b))>0
    def __set_flag(self,b,v):
        f = self.f&(~(1<<b)&0xFF)
        if v: f|=(1<<b)
        self.f=f
    @property
    def carry(self):
        return self.__get_flag(0)
    @carry.setter
    def carry(self,v):
        self.__set_flag(0,v)
    @property
    def data_segment_mode(self):
        return self.__get_flag(1)
    @data_segment_mode.setter
    def data_segment_mode(self,v):
        self.__set_flag(1,v)
    @property
    def overflow(self):
        return self.__get_flag(2)
    @overflow.setter
    def overflow(self,v):
        self.__set_flag(2,v)
    @property
    def parity(self):
        return self.__get_flag(2)
    @parity.setter
    def parity(self,v):
        self.__set_flag(2,v)
    @property
    def segment_adjust(self):
        return self.__get_flag(3)
    @segment_adjust.setter
    def segment_adjust(self,v):
        self.__set_flag(3,v)
    @property
    def half_carry(self):
        return self.__get_flag(4)
    @half_carry.setter
    def half_carry(self,v):
        self.__set_flag(4,v)
    @property
    def master_interrupt_enable(self):
        return self.__get_flag(5)
    @master_interrupt_enable.setter
    def master_interrupt_enable(self,v):
        self.__set_flag(5,v)
    @property
    def zero(self):
        return self.__get_flag(6)
    @zero.setter
    def zero(self,v):
        self.__set_flag(6,v)
    @property
    def sign(self):
        return self.__get_flag(7)
    @sign.setter
    def sign(self,v):
        self.__set_flag(7,v)


from abc import ABC, abstractmethod
class Memory(ABC):
    @abstractmethod
    def read(self,addr):
        """Reads a byte from address addr."""
        pass
    @abstractmethod
    def write(self,addr,val):
        """Writes byte val to address addr."""
        pass
    # helpers to read/write values with the right semantics
    # u8: just an alias for the read/write funcs
    def read_u8(self,addr):
        """Reads an unsigned 8-bit integer from address addr."""
        return self.read(addr)&0xFF
    def write_u8(self,addr,val):
        """Writes an unsigned 8-bit integer val at address addr."""
        self.write(addr,val&0xFF)
    # s8: two's complement
    def read_s8(self,addr):
        """Reads an signed 8-bit integer from address addr."""
        val = self.read(addr)&0xFF
        if (val&0x80):
            val = -0x80+(val&0x7F)
        return val
    def write_s8(self,addr,val):
        """Writes an signed 8-bit integer val at address addr."""
        self.write(addr,val&0xFF)
    # u16: little-endian 16 bit integer, must be word-aligned
    def read_u16(self,addr):
        """
        Reads an unsigned 16-bit integer from address addr, enforcing word
        alignment.
        """
        addr&=0xFFFFFE # enforce word alignment by ditching bottom bit
        val = self.read(addr)&0xFF
        val |= (self.read(addr+1)&0xFF)<<8
        return val
    def write_u16(self,addr,val):
        """
        Writes an unsigned 16-bit integer val at address addr, enforcing word
        alignment.
        """
        addr&=0xFFFFFE # enforce word alignment by ditching bottom bit
        self.write(addr,val&0xFF)
        self.write(addr+1,(val>>8)&0xFF)
    # s16: two's complement
    def read_s16(self,addr):
        """
        Reads an signed 16-bit integer from address addr, enforcing word
        alignment.
        """
        # piggyback off of u16
        val = self.read_u16(addr)&0xFFFF
        if (val&0x8000):
            val = -0x8000+(val&0x7FFF)
        return val
    def write_s16(self,addr,val):
        """
        Writes an signed 16-bit integer val at address addr, enforcing word
        alignment.
        """
        # piggyback off of u16
        self.write_u16(addr,val&0xFFFF)
    # u24: little-endian 16 bit integer, must be word-aligned
    def read_u24(self,addr):
        """
        Reads an unsigned 24-bit integer from address addr, enforcing word
        alignment.
        """
        addr&=0xFFFFFE # enforce word alignment by ditching bottom bit
        val = self.read(addr)&0xFF
        val |= (self.read(addr+1)&0xFF)<<8
        val |= (self.read(addr+2)&0xFF)<<16
        # dummy read to a 4th byte, since the specs say 24 bit reads read 32 bit
        # values and discard last byte
        self.read(addr+3)
        return val
    def write_u24(self,addr,val):
        """
        Writes an unsigned 24-bit integer val at address addr, enforcing word
        alignment.
        """
        addr&=0xFFFFFE # enforce word alignment by ditching bottom bit
        self.write(addr,val&0xFF)
        self.write(addr+1,(val>>8)&0xFF)
        self.write(addr+2,(val>>16)&0xFF)
    # s24: two's complement
    def read_s24(self,addr):
        """
        Reads an signed 24-bit integer from address addr, enforcing word
        alignment.
        """
        # piggyback off of u24
        val = self.read_u24(addr)&0xFFFF
        if (val&0x800000):
            val = -0x800000+(val&0x7FFFFF)
        return val
    def write_s24(self,addr,val):
        """
        Writes an signed 24-bit integer val at address addr, enforcing word
        alignment.
        """
        # piggyback off of u24
        self.write_u24(addr,val&0xFFFFFF)

class IOPorts(ABC):
    @abstractmethod
    def read(self,port):
        """Reads an 8-bit value from IO port port."""
        pass
    @abstractmethod
    def write(self,port,value):
        """Writes an 8-bit value to IO port port."""
        pass

class PilotException(Exception):
    pass

class UnknownOpcodeException(PilotException):
    def __init__(self,opcode):
        super(UnknownOpcodeException,self).__init__(self,\
            f"Unknown opcode {opcode:04X} ({opcode:016b})")

class InvalidOperation(PilotException):
    pass

class RM(ABC):
    def __init__(self,cpu,*args):
        self.cpu=cpu
        self.args=args
    @abstractmethod
    def get(self):
        pass
    @abstractmethod
    def set(self,v):
        pass

class Immediate(RM):
    def get(self):
        return self.args[0]
    def set(self,val):
        raise InvalidOperation("Cannot use immediate value as the destination of an operation.")

class Address8(RM):
    def get(self,signed=False):
        if signed:
            return self.cpu.memory.read_s8(self.args[0])
        return self.cpu.memory.read_u8(self.args[0])
    def set(self,v,signed=False):
        if signed:
            return self.cpu.memory.write_s8(self.args[0],v)
        self.cpu.memory.write_u8(self.args[0],v)

class Address16(RM):
    def get(self,signed=False):
        if signed:
            return self.cpu.memory.read_s16(self.args[0])
        return self.cpu.memory.read_u16(self.args[0])
    def set(self,v):
        if signed:
            return self.cpu.memory.write_s16(self.args[0],v)
        self.cpu.memory.write_u16(self.args[0],v)

class Register(RM):
    def get(self):
        if self.args[0]=="sp":
            return self.cpu.system_registers.sp
        return getattr(self.cpu.main_registers,self.args[0])
    def set(self,v):
        if self.args[0]=="sp":
            self.cpu.system_registers.sp=v
            return
        if self.args[0][-2:] in ("sx","zx"):
            raise InvalidOperation("Cannot use sign-extended/zero-extended \
register as the destination of an operation")
        setattr(self.cpu.main_registers,self.args[0],v&0xFFFF)

class Pilot:
    def __init__(self,memory,ioports):
        self.main_registers = Registers()
        self.shadow_registers = Registers()
        self.system_registers = SystemRegisters()
        self.memory = memory
        self.ioports = ioports
        self.system_registers.pc = 0xFF0000 # reset vector
        self.halted = False
        self.stopped = False
        self.system_registers.master_interrupt_enable=True
        self.repeat_register = None
        self.repeat_immediate = None
    def trigger_interrupt(self,interrupt):
        if not self.system_registers.master_interrupt_enable: return
        if self.repeat_immediate is not None: return
        if self.halted: self.halted=False
        # TODO: handle calling the interrupt vector
    def run(self):
        """Runs a single instruction."""
        if self.stopped: return
        if self.halted: return
        instr = self.memory.read_u16(self.system_registers.pc)
        pc_original = self.system_registers.pc
        self.system_registers.pc+=2
        if (instr>>11)==0 and ((instr>>9)%2)==0: # EXS
            self.exs(instr)
        elif (instr>>11)==0 and ((instr>>9)%2)==1: # DJNZ
            self.djnz(instr)
        elif (instr>>4)==256:
            # m==0 : HALT/STOP/CCF/SDS
            # m==1 : DAA/SAO/DAS/SAU
            # m==2 : not used
            # m==3 : REPI {A/B/H/L}
            m = (instr&0b11)
            if m==2:
                raise UnknownOpcodeException(instr)
            n = (instr>>2)&0b11
            [
                [self.halt,self.stop,self.ccf,self.sds],
                [self.daa,self.sao,self.das,self.sau],
                [],
                [self.repi_reg,self.repi_reg,self.repi_reg,self.repi_reg]
            ][m][n](instr)
        elif (instr>>4)==257:
            self.repi_imm(instr)
        elif (instr>>5)==129:
            self.ldpca(instr)
        elif (instr>>5)==130:
            self.ldixa(instr)
        elif (instr>>5)==131:
            self.ldspa(instr)
        elif (instr>>7)==36: # MULU/DIVU/MULSW/DIVSW
            n = (instr>>5)&0b11
            [self.mulu,self.mulsw,self.divu,self.divsw][n](instr)
        elif (instr>>9)==8: # LD dest, {[C+]/F/[C]/C/[C-]/E}
            self.ld_cfe_src(instr)
        elif (instr>>9)==9: # LD {[C+]/F/[C]/C/[C-]/E}, src
            self.ld_cfe_dest(instr)
        elif (instr>>10)==5: # {RESF/DI/SETF/EI} imm
            n = (instr>>8)&0b11
            [self.resf,self.di,self.setf,self.ei][n](instr)
        elif (instr>>9)==16: # {INC/DEC} dest, imm+1
            if (instr>>8)&1:
                self.dec(instr)
            else:
                self.inc(instr)
        elif (instr>>9)==17: # {NEG/CPL/NGC/CLR} dest
            n = (instr>>7)&0b11
            [self.neg,self.cpl,self.ngc,self.clr][n](instr)
        elif (instr>>9)==18: # {INC/DEC}W dest, imm+1
            if (instr>>8)&1:
                self.decw(instr)
            else:
                self.incw(instr)
        elif (instr>>9)==19: # {NEG/CPL/NGC/CLR}W dest
            n = (instr>>7)&0b11
            [self.negw,self.cplw,self.ngcw,self.clrw][n](instr)
        elif (instr>>8)==48: # {RLC/RRC/RL/RR/SLA/SRA/SWAP/SRL} dest
            n = (instr>>5)&0b111
            [
                self.rlc,
                self.rrc,
                self.rl,
                self.rr,
                self.sla,
                self.sra,
                self.swap,
                self.srl
            ][n](instr)
        elif (instr>>8)==52: # {RLC/RRC/RL/RR/SLA/SRA/SWAP/SRL}W dest
            n = (instr>>5)&0b111
            [
                self.rlcw,
                self.rrcw,
                self.rlw,
                self.rrw,
                self.slaw,
                self.sraw,
                self.swapw,
                self.srlw
            ][n](instr)
        else: # if no instructions match, error
            raise UnknownOpcodeException(instr)
        # decrement repeat counter
        if self.repeat_register is not None:
            v = getattr(self.main_registers,self.repeat_register)
            setattr(self.main_registers,self.repeat_register,v-1)
        elif self.repeat_immediate is not None:
            self.repeat_immediate-=1
        # check repeat counter
        if self.repeat_register is not None and \
            getattr(self.main_registers,self.repeat_register)==0:
            self.repeat_register=None
        elif self.repeat_immediate is not None and self.repeat_immediate==0:
            self.repeat_immediate=None
        elif (self.repeat_register is not None) or \
            (self.repeat_immediate is not None):
            self.system_registers.pc=pc_original
    def exs(self,instr):
        if instr==0: return # NOP is just a special case of EXS
        if instr&0x0003==0x0003: # AB (optimize for if you want to switch both)
            tmp = self.shadow_registers.ab
            self.shadow_registers.ab = self.main_registers.ab
            self.main_registers.ab = tmp
        elif instr&0x0001: # B
            tmp = self.shadow_registers.b
            self.shadow_registers.b = self.main_registers.b
            self.main_registers.b = tmp
        elif instr&0x0002: # A
            tmp = self.shadow_registers.a
            self.shadow_registers.a = self.main_registers.a
            self.main_registers.a = tmp
        if instr&0x000C==0x000C: # HL (same as AB)
            tmp = self.shadow_registers.hl
            self.shadow_registers.hl = self.main_registers.hl
            self.main_registers.hl = tmp
        elif instr&0x0004: # L
            tmp = self.shadow_registers.l
            self.shadow_registers.l = self.main_registers.l
            self.main_registers.l = tmp
        elif instr&0x0008: # H
            tmp = self.shadow_registers.h
            self.shadow_registers.h = self.main_registers.h
            self.main_registers.h = tmp
        if instr&0x0030==0x0030: # IX (same as AB)
            tmp = self.shadow_registers.ix
            self.shadow_registers.ix = self.main_registers.ix
            self.main_registers.ix = tmp
        elif instr&0x0010: # X
            tmp = self.shadow_registers.x
            self.shadow_registers.x = self.main_registers.x
            self.main_registers.x = tmp
        elif instr&0x0020: # I
            tmp = self.shadow_registers.i
            self.shadow_registers.i = self.main_registers.i
            self.main_registers.i = tmp
        if instr&0x00C0==0x00C0: # DS (same as AB)
            tmp = self.shadow_registers.ds
            self.shadow_registers.ds = self.main_registers.ds
            self.main_registers.ds = tmp
        elif instr&0x0040: # S
            tmp = self.shadow_registers.s
            self.shadow_registers.s = self.main_registers.s
            self.main_registers.s = tmp
        elif instr&0x0080: # D
            tmp = self.shadow_registers.d
            self.shadow_registers.d = self.main_registers.d
            self.main_registers.d = tmp
        if instr&0x0100: # C
            tmp = self.shadow_registers.c
            self.shadow_registers.c = self.main_registers.c
            self.main_registers.c = tmp
        if instr&0x0400: # F (flags)
            tmp = self.system_registers.shadow_f
            self.system_registers.shadow_f = self.system_registers.f&0b11011111
            self.system_registers.f = tmp|(self.system_registers.f&0b00100000)
    def djnz(self,instr):
        branch=False
        if instr&(1<<10):
            if instr&(1<<8):
                self.main_registers.hl=(self.main_registers.hl-1)&0xFFFF
                branch=(self.main_registers.hl>0)
            else:
                self.main_registers.ab=(self.main_registers.ab-1)&0xFFFF
                branch=(self.main_registers.ab>0)
        else:
            if instr&(1<<8):
                self.main_registers.b=(self.main_registers.b-1)&0xFF
                branch=(self.main_registers.b>0)
            else:
                self.main_registers.a=(self.main_registers.a-1)&0xFF
                branch=(self.main_registers.a>0)
        if branch:
            # note: since pcr8 is one-extended (and therefore always negative),
            # this is functionally a subtraction, but since pcr8 returns the
            # offset as a negative number we add it anyways
            self.system_registers.pc_only+=self.pcr8(instr&0xFF)
    def halt(self,instr):
        self.halted=True
    def stop(self,instr):
        self.stopped=True
    def ccf(self,instr):
        self.system_registers.carry = not self.system_registers.carry
    def sds(self,instr):
        self.main_registers.d=self.system_registers.k
        self.system_registers.data_segment_mode=True
    def daa(self,instr):
        adj = 0x00
        if self.system_registers.half_carry or (self.main_registers.a&0xF)>0x9:
            adj|=0x06
        if self.system_registers.carry or (self.main_registers.a&0xF0)>0x90:
            adj|=0x60
            self.system_registers.carry=True
        a = self.main_registers.a # for use later
        self.main_registers.a=(a+adj)
        new_a = self.main_registers.a
        self.system_registers.zero = new_a==0
        # to quote the fast inverse square root code: "what the fuck?"
        # basically do some black magic to detect half-carries and determine
        # parity
        # ...or not, because Spweesh has decided they shouldn't be done
        # still keeping these here because they're awesome black magic
        # self.system_registers.half_carry = (new_a^a^adj)==(1<<4)
        # self.system_registers.parity = (0x9669>>(new_a>>4^(new_a&0x0F)))&1
    def sao(self,instr):
        if self.system_registers.segment_adjust:
            self.main_registers.d+=1
    def das(self,instr):
        adj = 0x00
        if self.system_registers.half_carry or (self.main_registers.a&0xF)>0x9:
            adj|=0x06
        if self.system_registers.carry or (self.main_registers.a&0xF0)>0x90:
            adj|=0x60
            self.system_registers.carry=True
        a = self.main_registers.a # for use later
        self.main_registers.a=(a-adj)
        new_a = self.main_registers.a
        self.system_registers.zero = new_a==0
        # to quote the fast inverse square root code: "what the fuck?"
        # basically do some black magic to detect half-carries and determine
        # parity
        # ...or not, because Spweesh has decided they shouldn't be done
        # still keeping these here because they're awesome black magic
        # self.system_registers.half_carry = (new_a^a^adj)==(1<<4)
        # self.system_registers.parity = (0x9669>>(new_a>>4^(new_a&0x0F)))&1
    def sau(self,instr):
        if self.system_registers.segment_adjust:
            self.main_registers.d-=1
    def repi_reg(self,instr):
        self.repeat_register = "abhl"[(instr>>2)&0b11]
    def repi_imm(self,instr):
        self.repeat_immediate = 2 + (instr&0xF)
    def ldpca(self,instr):
        offset = self.memory.read_u16(self.system_registers.pc)
        self.system_registers.pc+=2
        dest = self.getrm(instr&31,True)
        val = self.system_registers.pc_only+self.pcr16(offset)
        dest.set(val)
        self.main_registers.d=self.system_registers.k
        self.system_registers.data_segment_mode=True
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31,True)
    def ldixa(self,instr):
        offset = self.memory.read_s16(self.system_registers.pc)
        self.system_registers.pc+=2
        dest = self.getrm(instr&31,True)
        dest.set(self.system_registers.ix+offset)
        val=dest.get()
        self.finalizerm(instr&31,True)
        adj=False
        if offset>0 and (val<self.system_registers.ix):
            adj=True
        elif offset<0 and (val>self.system_registers.ix):
            adj=True
        self.system_registers.segment_adjust=adj
    def ldspa(self,instr):
        offset = self.memory.read_u16(self.system_registers.pc)
        self.system_registers.pc+=2
        dest = self.getrm(instr&31,True)
        dest.set(self.system_registers.sp+offset)
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31,True)
    def mulu(self,instr):
        src = self.getrm(instr&31)
        self.main_registers.ab = self.main_registers.a*src.get()
        self.system_registers.segment_adjust=False
        self.finalize_rm(instr&31)
        self.system_registers.sign = (self.main_registers.ab&0x8000)>0
        self.system_registers.zero = (self.main_registers.ab==0)
        self.system_registers.overflow = False
    def divu(self,instr):
        src = self.getrm(instr&31)
        numerator = self.main_registers.ab
        divisor = src.get()
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31)
        if divisor==0:
            self.system_registers.overflow = False
            self.system_registers.zero = True
            return
        quotient = numerator // divisor
        remainder = numerator % divisor
        self.main_registers.a = remainder
        self.main_registers.b = quotient
        self.system_registers.overflow = quotient>0xFF
    def mulsw(self,instr):
        src = self.getrm(instr&31,True)
        ab_signed = self.pcr16(self.main_registers.ab)
        self.main_registers.abhl=ab_signed*src.get(True)
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31,True)
        self.system_registers.sign = (self.main_registers.abhl&0x80000000)>0
        self.system_registers.zero = (self.main_registers.abhl==0)
        self.system_registers.overflow = False
    def divsw(self,instr):
        src = self.getrm(instr&31,True)
        numerator = self.to_s32(self.main_registers.abhl)
        divisor = src.get(True)
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31,True)
        if divisor==0:
            self.system_registers.overflow = False
            self.system_registers.zero = True
            return
        quotient = numerator // divisor
        remainder = numerator % divisor
        self.main_registers.ab = remainder
        self.main_registers.hl   = quotient
        self.system_registers.overflow = quotient>0xFFFF
    def ld_cfe_src(self,instr):
        src = (instr>>6)&0b111
        dest = self.getrm(instr&31)
        if src in (2,4,6):
            dest.set(self.ioports.read(self.main_registers.c))
            if src==2:
                self.main_registers.c=(self.main_registers.c+1)&0xFF
            elif src==6:
                self.main_registers.c=(self.main_registers.c-1)&0xFF
        elif src==3:
            dest.set(self.system_registers.f)
        elif src==5:
            dest.set(self.main_registers.c)
        elif src==7:
            dest.set(self.system_registers.ie)
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31)
    def ld_cfe_dest(self,instr):
        dest = (instr>>6)&0b111
        src = self.getrm(instr&31)
        if dest in (2,4,6):
            self.main_registers.c=src.get()
            if src==2:
                self.main_registers.c=(self.main_registers.c+1)&0xFF
            elif src==6:
                self.main_registers.c=(self.main_registers.c-1)&0xFF
        elif dest==3:
            self.system_registers.f=src.get()
        elif dest==5:
            self.main_registers.c=src.get()
        elif dest==7:
            self.system_registers.ie=src.get()
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31)
    def resf(self,instr):
        self.system_registers.f&=(~instr&0xFF)
    def di(self,instr):
        self.system_registers.ie&=(~instr&0xFF)
    def setf(self,instr):
        self.system_registers.f|=(instr&0xFF)
    def ei(self,instr):
        self.system_registers.ie|=(instr&0xFF)
    def inc(self,instr):
        dest = self.getrm(instr&31)
        imm = (instr>>5)&7
        imm += 1
        og = dest.get()
        dest.set(og+imm)
        val = dest.get()
        self.system_registers.half_carry = ((val^og^imm)&(1<<4)))==(1<<4)
        self.system_registers.zero = val==0
        self.system_registers.sign = (val&0x80)>0
        if (val&0x80):
            self.system_registers.overflow = (og&0x80)==0 and (imm&0x80)==0
        else:
            self.system_registers.overflow = (og&0x80)>0 and (imm&0x80)>0
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31)
    def dec(self,instr):
        dest = self.getrm(instr&31)
        imm = (instr>>5)&7
        imm += 1
        og = dest.get()
        dest.set(og-imm)
        val = dest.get()
        self.system_registers.half_carry = ((val^og^imm)&(1<<4)))==(1<<4)
        self.system_registers.zero = val==0
        self.system_registers.sign = (val&0x80)>0
        if (imm&0x80):
            self.system_registers.overflow = (val&0x80)>0 and (og&0x80)==0
        else:
            self.system_registers.overflow = (val&0x80)==0 and (og&0x80)>0
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31)
    def neg(self,instr):
        dest = self.getrm(instr&31)
        og = dest.get()
        dest.set(-og)
        val = dest.get()
        self.finalizerm(instr&31)
        self.system_registers.carry = val>0
        self.system_registers.segment_adjust = self.system_registers.carry
        self.system_registers.half_carry = ((val^0^og)&(1<<4)))==(1<<4)
        self.system_registers.zero = val==0
        self.system_registers.sign = (val&0x80)>0
        self.system_registers.overflow = ((og&0x80)>0 and (og&0x80)==(val&0x80))
    def cpl(self,instr):
        dest = self.getrm(instr&31)
        og = dest.get()
        dest.set(~og)
        val = (~og)&0xFF
        self.system_registers.sign=(val&0x80)>0
        self.system_registers.zero=(val==0)
        self.system_registers.parity = (0x9669>>(val>>4^(val&0x0F)))&1
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31)
    def ngc(self,instr):
        dest = self.getrm(instr&31)
        og = dest.get()+int(self.system_registers.carry)
        dest.set(-og)
        val = dest.get()
        self.finalizerm(instr&31)
        self.system_registers.carry = val>0
        self.system_registers.segment_adjust = self.system_registers.carry
        self.system_registers.half_carry = ((val^0^og)&(1<<4)))==(1<<4)
        self.system_registers.zero = val==0
        self.system_registers.sign = (val&0x80)>0
        self.system_registers.overflow = ((og&0x80)>0 and (og&0x80)==(val&0x80))
    def clr(self,instr):
        dest = self.getrm(instr&31)
        dest.set(0)
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31)
    def incw(self,instr):
        dest = self.getrm(instr&31,True)
        og = dest.get()
        imm = (instr>>5)&7
        imm += 1
        dest.set(og+imm)
        if type(dest,Register):
            self.finalizerm(instr&31,True)
            # replace Segment Adjust with a carry flag
            self.system_registers.segment_adjust = (dest.get()<og)
        else:
            self.system_registers.half_carry = ((val^og^imm)&(1<<4)))==(1<<4)
            self.system_registers.zero = val==0
            self.system_registers.sign = (val&0x8000)>0
            if (val&0x8000):
                self.system_registers.overflow = (og&0x8000)==0 and (imm&0x8000)==0
            else:
                self.system_registers.overflow = (og&0x8000)>0 and (imm&0x8000)>0
            self.system_registers.segment_adjust=False
            self.finalizerm(instr&31,True)
    def decw(self,instr):
        dest = self.getrm(instr&31,True)
        og = dest.get()
        imm = (instr>>5)&7
        imm += 1
        dest.set(og-imm)
        if type(dest,Register):
            self.finalizerm(instr&31,True)
            # replace Segment Adjust with a carry flag
            self.system_registers.segment_adjust = (dest.get()>og)
        else:
            self.system_registers.half_carry = ((val^og^imm)&(1<<4)))==(1<<4)
            self.system_registers.zero = val==0
            self.system_registers.sign = (val&0x8000)>0
            if (imm&0x8000):
                self.system_registers.overflow = (val&0x8000)>0 and (og&0x8000)==0
            else:
                self.system_registers.overflow = (val&0x8000)==0 and (og&0x8000)>0
            self.system_registers.segment_adjust=False
            self.finalizerm(instr&31,True)
    def negw(self,instr):
        dest = self.getrm(instr&31,True)
        og = dest.get()
        dest.set(-og)
        val = dest.get()
        self.finalizerm(instr&31,True)
        self.system_registers.carry = val>0
        self.system_registers.segment_adjust = self.system_registers.carry
        self.system_registers.half_carry = (((val>>8)^0^(og>>8))&(1<<4)))==(1<<4)
        self.system_registers.zero = val==0
        self.system_registers.sign = (val&0x8000)>0
        self.system_registers.overflow = ((og&0x8000)>0 and (og&0x8000)==(val&0x8000))
    def cplw(self,instr):
        dest = self.getrm(instr&31,True)
        og = dest.get()
        dest.set(~og)
        val = (~og)&0xFF
        self.system_registers.sign=(val&0x8000)>0
        self.system_registers.zero=(val==0)
        self.system_registers.parity = (0x9669>>((val>>12&0x0F)^(val>>8&0x0F)^\
            (val>>4&0x0F)^(val&0x0F)))&1
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31,True)
    def ngcw(self,instr):
        dest = self.getrm(instr&31,True)
        og = dest.get()+int(self.system_registers.carry)
        dest.set(-og)
        val = dest.get()
        self.finalizerm(instr&31,True)
        self.system_registers.carry = val>0
        self.system_registers.segment_adjust = self.system_registers.carry
        self.system_registers.half_carry = (((val>>8)^0^(og>>8))&(1<<4)))==(1<<4)
        self.system_registers.zero = val==0
        self.system_registers.sign = (val&0x8000)>0
        self.system_registers.overflow = ((og&0x8000)>0 and (og&0x8000)==(val&0x8000))
    def clrw(self,instr):
        dest = self.getrm(instr&31,True)
        dest.set(0)
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31,True)
    def rlc(self,instr):
        dest = self.getrm(instr&31)
        og = dest.get()
        val = (og<<1&0xFF)|(og>>7&1)
        dest.set(val)
        self.finalizerm(instr&31)
        self.system_registers.sign=(val&0x80)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og>>7)&1
        self.system_registers.segment_adjust=self.system_registers.carry
    def rrc(self,instr):
        dest = self.getrm(instr&31)
        og = dest.get()
        val = (og>>1&0x7F)|((og&1)<<7)
        dest.set(val)
        self.finalizerm(instr&31)
        self.system_registers.sign=(val&0x80)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og)&1
        self.system_registers.segment_adjust=self.system_registers.carry
    def rl(self,instr):
        dest = self.getrm(instr&31)
        og = dest.get()
        val = (og<<1&0xFF)|self.system_registers.carry
        dest.set(val)
        self.finalizerm(instr&31)
        self.system_registers.sign=(val&0x80)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og>>7)&1
        self.system_registers.segment_adjust=self.system_registers.carry
    def rr(self,instr):
        dest = self.getrm(instr&31)
        og = dest.get()
        val = (og>>1&0x7F)|(self.system_registers.carry<<7)
        dest.set(val)
        self.finalizerm(instr&31)
        self.system_registers.sign=(val&0x80)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og)&1
        self.system_registers.segment_adjust=self.system_registers.carry
    def sla(self,instr):
        dest = self.getrm(instr&31)
        og = dest.get()
        val = (og<<1)&0xFF
        dest.set(val)
        self.finalizerm(instr&31)
        self.system_registers.sign=(val&0x80)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og>>7)&1
        self.system_registers.segment_adjust=self.system_registers.carry
        self.system_registers.overflow=(og&0x80)!=(val&0x80)
    def sra(self,instr):
        dest = self.getrm(instr&31)
        og = dest.get()
        val = ((og>>1)&0x7F) | (og&0x80)
        dest.set(val)
        self.finalizerm(instr&31)
        self.system_registers.sign=(val&0x80)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og)&1
        self.system_registers.segment_adjust=self.system_registers.carry
        self.system_registers.overflow=False
    def swap(self,instr):
        dest = self.getrm(instr&31)
        og = dest.get()
        val = (og<<4)&0xF0 | (og>>4)&0x0F
        dest.set(val)
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31)
        self.system_registers.sign=(val&0x80)>0
        self.system_registers.zero=(val==0)
        # also modifies overflow but not sure how
    def srl(self,instr):
        dest = self.getrm(instr&31)
        og = dest.get()
        val = (og>>1)&0x7F
        dest.set(val)
        self.finalizerm(instr&31)
        self.system_registers.sign=(val&0x80)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og)&1
        self.system_registers.segment_adjust=self.system_registers.carry
    def rlcw(self,instr):
        dest = self.getrm(instr&31,True)
        og = dest.get()
        val = (og<<1&0xFF)|(og>>15&1)
        dest.set(val)
        self.finalizerm(instr&31,True)
        self.system_registers.sign=(val&0x8000)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og>>15)&1
        self.system_registers.segment_adjust=self.system_registers.carry
    def rrcw(self,instr):
        dest = self.getrm(instr&31,True)
        og = dest.get()
        val = (og>>1&0x7FFF)|((og&1)<<15)
        dest.set(val)
        self.finalizerm(instr&31,True)
        self.system_registers.sign=(val&0x8000)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og)&1
        self.system_registers.segment_adjust=self.system_registers.carry
    def rlw(self,instr):
        dest = self.getrm(instr&31,True)
        og = dest.get()
        val = (og<<1&0xFFFF)|self.system_registers.carry
        dest.set(val)
        self.finalizerm(instr&31,True)
        self.system_registers.sign=(val&0x8000)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og>>15)&1
        self.system_registers.segment_adjust=self.system_registers.carry
    def rrw(self,instr):
        dest = self.getrm(instr&31,True)
        og = dest.get()
        val = (og>>1&0x7FFF)|(self.system_registers.carry<<15)
        dest.set(val)
        self.finalizerm(instr&31,True)
        self.system_registers.sign=(val&0x8000)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og)&1
        self.system_registers.segment_adjust=self.system_registers.carry
    def slaw(self,instr):
        dest = self.getrm(instr&31,True)
        og = dest.get()
        val = (og<<1)&0xFFFF
        dest.set(val)
        self.finalizerm(instr&31,True)
        self.system_registers.sign=(val&0x8000)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og>>15)&1
        self.system_registers.segment_adjust=self.system_registers.carry
        self.system_registers.overflow=(og&0x80)!=(val&0x80)
    def sraw(self,instr):
        dest = self.getrm(instr&31,True)
        og = dest.get()
        val = ((og>>1)&0x7FFF) | (og&0x8000)
        dest.set(val)
        self.finalizerm(instr&31,True)
        self.system_registers.sign=(val&0x8000)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og)&1
        self.system_registers.segment_adjust=self.system_registers.carry
        self.system_registers.overflow=False
    def swapw(self,instr):
        dest = self.getrm(instr&31,True)
        og = dest.get()
        val = (og<<8)&0xFF00 | (og>>8)&0x00FF
        dest.set(val)
        self.system_registers.segment_adjust=False
        self.finalizerm(instr&31,True)
        self.system_registers.sign=(val&0x80)>0
        self.system_registers.zero=(val==0)
    def srlw(self,instr):
        dest = self.getrm(instr&31,True)
        og = dest.get()
        val = (og>>1)&0x7FFF
        dest.set(val)
        self.finalizerm(instr&31,True)
        self.system_registers.sign=(val&0x80)>0
        self.system_registers.zero=(val==0)
        self.system_registers.carry=(og)&1
        self.system_registers.segment_adjust=self.system_registers.carry
    def getrm(self,v,sixteen=False):
        if v==0: # imm
            val = self.memory.read_u16(self.system_registers.pc)
            self.system_registers.pc+=2
            return Immediate(self,val)
        elif v&4==3: # registers
            reg = ""
            if v==3: # A/AB
                reg = "ab" if sixteen else "a"
            elif v==7: # B/ASX
                reg = "asx" if sixteen else "b"
            elif v==11: # H/HL
                reg = "hl" if sixteen else "h"
            elif v==15: # L/BSX
                reg = "bsx" if sixteen else "l"
            elif v==19: # I/IX
                reg = "ix" if sixteen else "i"
            elif v==23: # X/ASX
                reg = "azx" if sixteen else "x"
            elif v==27: # D/DS
                reg = "ds" if sixteen else "d"
            elif v==31: # S/SP
                reg = "sp" if sixteen else "s"
            return Register(self,reg)
        elif v in (28,30): # unused rm specs
            raise InvalidOperation(f"unknown rm specifier {v}")
        else: #addresses
            addr = 0
            if v&4==0: # these take arguments
                arg = self.memory.read_u16(self.system_registers.pc)
                self.system_registers.pc+=2
                if v==4: # [$01xxxx]
                    addr = 0x010000|arg
                elif v==8: # [$00xxxx]
                    addr = 0x000000|arg
                elif v==12: # DS:[$xxxx]
                    if self.system_registers.data_segment_mode:
                        addr = (self.main_registers.d<<16)|arg
                    else:
                        addr = (self.main_registers.ds<<8)+arg
                elif v==16: # [IX+imm]
                    addr = (self.main_registers.ix+arg)&0xFFFF
                elif v==20: # DS:[IX+imm]
                    arg = (self.main_registers.ix+arg)&0xFFFF
                    if self.system_registers.data_segment_mode:
                        addr = (self.main_registers.d<<16)|arg
                    else:
                        addr = (self.main_registers.ds<<8)+arg
                elif v==24: # [SP+imm]
                    addr = (self.system_registers.sp+arg)&0xFFFF
            elif v in (1,9,10): # [HL(+/-/(nothing))]
                addr = self.main_registers.hl
            elif v in (17,18,25): # [IX(+/-/(nothing))]
                addr = self.main_registers.ix
            elif v in (5,13,14):# BS:[HL(+/-/(nothing))]
                arg = self.main_registers.hl
                if self.system_registers.data_segment_mode:
                    addr = (self.main_registers.d<<16)|arg
                else:
                    addr = (self.main_registers.ds<<8)+arg
            elif v in (21,22,29): # BS:[IX(+/-/(nothing))]
                arg = self.main_registers.ix
                if self.system_registers.data_segment_mode:
                    addr = (self.main_registers.d<<16)|arg
                else:
                    addr = (self.main_registers.ds<<8)+arg
            elif v in (2,6): # (DS:)[AB]
                addr = self.main_registers.ab
                if v==6:
                    if self.system_registers.data_segment_mode:
                        addr = (self.main_registers.d<<16)|addr
                    else:
                        addr = (self.main_registers.ds<<8)+addr
            elif v==24:
                addr = self.main_registers.ds
                # there is no DS:[DS] because... think about it
            if sixteen:
                return Address16(self,addr)
            else:
                return Address8(self,addr)
        raise PilotException(f"something's wrong here (rm specifier {v})")
    def finalizerm(self,v,sixteen=False):
        hl = self.main_registers.hl
        ix = self.main_registers.ix
        if v in (1,5):
            self.main_registers.hl=(self.main_registers.hl+1)&0xFFFF
            if v==5 and self.main_registers.hl<hl:
                self.system_registers.segment_adjust=True
        elif v in (9,13):
            self.main_registers.hl=(self.main_registers.hl-1)&0xFFFF
            if v==13 and self.main_registers.hl>hl:
                self.system_registers.segment_adjust=True
        elif v in (17,21):
            self.main_registers.ix=(self.main_registers.ix+1)&0xFFFF
            if v==21 and self.main_registers.ix<ix:
                self.system_registers.segment_adjust=True
        elif v in (25,29):
            self.main_registers.ix=(self.main_registers.ix-1)&0xFFFF
            if v==29 and self.main_registers.ix>ix:
                self.system_registers.segment_adjust=True
        if sixteen: # do it twice for sixteen-bit reads/writes
            self.finalizerm(v)
    def pcr8(self,val):
        """Converts pcr8 val into a valid PC offset."""
        val&=0xFF # 8 bit value
        return self.pcr15(0x7F00|val)
    def pcr10(self,val):
        """Converts pcr10 val into a valid PC offset."""
        val&=0x3FF # 10 bit value
        sign_bit = 1<<9
        # don't worry about a negative number getting into pcr15, it does a
        # bitwise and which re-unsigns the integer
        return self.pcr15((val&(sign_bit-1))-(val&sign_bit))
    def pcr15(self,val):
        """Converts pcr15 val into a valid PC offset."""
        val&=0x7FFF # 15 bit value
        return self.pcr16(val<<1)
    def pcr16(self,val):
        """Converts pcr16 val into a valid PC offset."""
        val&=0xFFFF # 16 bit value
        if (val&0x8000):
            val = -0x8000+(val&0x7FFF)
        return val
    def to_s32(self,val):
        """Converts unsigned 32 bit value to signed 32 bit value."""
        val&=0xFFFFFFFF # 32 bit value
        if (val&0x80000000):
            val = -0x80000000+(val&0x7FFFFFFF)
        return val
