#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>
#include <linux/hardirq.h>
#include <linux/preempt.h>
#include <linux/sched.h>

void inline measured_function(volatile int *var)
{
  (*var) = 1;
}

static void vmcall(void)
{
  unsigned long a = 0, b, c, d;
  asm volatile ("vmcall" : "+a"(a), "=b"(b), "=c"(c), "=d"(d));
}


/*
static void cpuid(void)
{
  asm volatile ("push %%"R "bx; cpuid; pop %%"R "bx"
		: : : "eax", "ecx", "edx");
}
*/

static int __init hello_start(void)
{
  unsigned long flags;
  uint64_t start, end;
  int variable = 0;
  unsigned cycles_low, cycles_high, cycles_low1, cycles_high1;
  int nexp;
  uint64_t res;

  printk(KERN_INFO "Loading test module...\n");
  
  for(nexp=0;nexp<10; nexp++) {
    preempt_disable(); /*we disable preemption on our CPU*/
    raw_local_irq_save(flags); /*we disable hard interrupts on our CPU*/
    /*at this stage we exclusively own the CPU*/
    asm volatile (
		  "RDTSC\n\t"
		  "mov %%edx, %0\n\t"
		  "mov %%eax, %1\n\t": "=r" (cycles_high), "=r" (cycles_low));
    
    // measured_function(&variable);
    vmcall(); 
    //cpuid();
    
    asm volatile (
		  "RDTSC\n\t"
		  "mov %%edx, %0\n\t"
		  "mov %%eax, %1\n\t": "=r" (cycles_high1), "=r" (cycles_low1));
    raw_local_irq_restore(flags);
    /*we enable hard interrupts on our CPU*/
    preempt_enable();/*we enable preemption*/
    start = ( ((uint64_t)cycles_high << 32) | cycles_low );
    end = ( ((uint64_t)cycles_high1 << 32) | cycles_low1 );
    
    res = (end-start);
    printk(KERN_INFO "\n[%d] FUCK YOU function execution time is %llu clock cycles", nexp, res);
    
    
  }
  
  
  return 0;
}

static void __exit hello_end(void)
{
  printk(KERN_INFO "Goodbye Mr.\n");
}
module_init(hello_start);
module_exit(hello_end);
